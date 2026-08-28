{-# LANGUAGE TupleSections #-}

module Type.Annotation (annotateType, inferType) where

import Annotation (annotation)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (foldM, unless)
import Control.Monad.State
import Control.Monad.Writer (censor, listen)
import Data.Foldable (find)
import qualified Data.Set as Set
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Diagnostic (range), Severity (Error), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import qualified Extension.Core as Extension
import qualified SyntaxGen.AbsStella as AST
import Type.Application (annotateAbstractionType, annotateApplicationType)
import qualified Type.Constraint as Constraint
import Type.Context (withName)
import qualified Type.Context as Context
import Type.Core (Type (Type), list)
import qualified Type.Core as Type
import Type.Decl (withDecls, withParamDecls)
import Type.Env (TypeAnnotationEnv, isAvailable, positionOf, tellC, tellD, typeOf, withStateTAE)
import Type.Exception (annotateExceptionExprType)
import Type.Expectation (TypeKind (Expected, Inferred), ensureEqType, listItemType, mismatchSS, sanitizeT, sanitizeTSilent, validateTypeParameters)
import Type.Expression (annotateTT2B, annotateTT2T)
import Type.Lift (liftType, liftType')
import Type.Match (annotateLetType, annotateMatchType)
import Type.Record (annotateDotRecordType, annotateRecordType)
import Type.Reference (annotateRefExprType)
import qualified Type.Substitution as Substitution
import Type.Sum (annotateSumExprType)
import Type.Tuple (annotateDotTupleType, annotateTupleType)
import qualified Type.Unification as Unification
import Type.Variant (variantExprTyping, variantFieldTyping)

class TypeAnnotatable f where
  annotateType :: Maybe Type -> f Position -> TypeAnnotationEnv (f (Position, Maybe Type))

checkType :: (TypeAnnotatable f) => Type -> f Position -> TypeAnnotationEnv (f (Position, Maybe Type))
checkType t = annotateType $ Just t

inferType :: (TypeAnnotatable f) => f Position -> TypeAnnotationEnv (f (Position, Maybe Type))
inferType = annotateType Nothing

data EscapePolicy = DiscardEscaping | RejectEscaping

solveScopedConstraints ::
  Position ->
  Set.Set String ->
  Set.Set String ->
  EscapePolicy ->
  (Substitution.Substitution -> a -> a) ->
  TypeAnnotationEnv a ->
  TypeAnnotationEnv a
solveScopedConstraints p externalMetaVariables rigidVariables escapePolicy applySubstitution annotate = do
  (annotated, (_, constraints)) <-
    censor (\(diagnostics, _) -> (diagnostics, mempty)) $
      listen annotate
  metaVariables <- gets Context.metaVars
  case Unification.unify metaVariables constraints of
    Left issue -> do
      tellD [issue]
      pure annotated
    Right substitution -> do
      let localMetaVariables = metaVariables `Set.difference` externalMetaVariables
          classify name =
            let variable = Type $ AST.TypeVar () (AST.StellaIdent name)
                inferred = Substitution.apply substitution variable
                escapesScope =
                  not (Set.null $ Type.fv inferred `Set.intersection` rigidVariables)
                    || not (Set.null $ Unification.freeMetaVars localMetaVariables inferred)
             in (variable, inferred, escapesScope)
          solutions = filter (\(variable, inferred, _) -> not $ Unification.alphaEq variable inferred) $ classify <$> Set.toList externalMetaVariables
          safeConstraints = [Constraint.Eq p variable inferred | (variable, inferred, False) <- solutions]
          escapingSolutions = [(variable, inferred) | (variable, inferred, True) <- solutions]
      tellC safeConstraints
      case escapePolicy of
        DiscardEscaping -> pure ()
        RejectEscaping -> unless (null escapingSolutions) $ do
          let message = "type inference solution escapes universal scope: " ++ show escapingSolutions
          tellD [diagnostic Error UNEXPECTED_TYPE_FOR_EXPRESSION (pointRange p) message]
      pure $ applySubstitution substitution annotated

applyTypeAnnotation :: Substitution.Substitution -> (Position, Maybe Type) -> (Position, Maybe Type)
applyTypeAnnotation substitution (position, type_) =
  (position, Substitution.apply substitution <$> type_)

annotateFunction ::
  Maybe Type ->
  Position ->
  [AST.Annotation' Position] ->
  String ->
  [AST.ParamDecl' Position] ->
  AST.ReturnType' Position ->
  AST.ThrowType' Position ->
  [AST.Decl' Position] ->
  AST.Expr' Position ->
  TypeAnnotationEnv
    ( Maybe Type,
      [AST.ParamDecl' (Position, Maybe Type)],
      [AST.Decl' (Position, Maybe Type)],
      AST.Expr' (Position, Maybe Type)
    )
annotateFunction declaredType p annotations fname paramdecls returntype throwtype decls expr = do
  unless (null annotations) $ tellD [notImplemented p "DeclFun annotations"]

  contextWithParams <- gets (withName fname) >>= withDecls decls {-isTopLevel=-} False >>= withParamDecls paramdecls

  let declaredSignature = case declaredType of
        Just (Type (AST.TypeFun () args result)) -> Just (fmap Type args, Type result)
        _ -> Nothing
      context' = case declaredSignature of
        Just (args, _)
          | length args == length paramdecls ->
              let names = [name | AST.AParamDecl _ (AST.StellaIdent name) _ <- paramdecls]
               in foldr (uncurry Context.withTyped) contextWithParams (zip names args)
        _ -> contextWithParams

  case throwtype of
    AST.NoThrowType _ -> pure ()
    AST.SomeThrowType _ _ -> tellD [notImplemented p "DeclFun ThrowType"]

  let annotateParam (AST.AParamDecl p' (AST.StellaIdent name) type_) =
        AST.AParamDecl (p', Context.typeOf name context') (AST.StellaIdent name) (stub type_)
      paramdecls' = fmap annotateParam paramdecls

  expectedReturn <- case declaredSignature of
    Just (_, result) -> pure $ Just result
    Nothing -> case returntype of
      AST.SomeReturnType _ type_ -> Just <$> sanitizeTSilent type_
      AST.NoReturnType _ -> pure Nothing
  (decls', expr') <- withStateTAE (const context') $ do
    decls' <- mapM inferType decls
    expr' <- annotateType expectedReturn expr
    pure (decls', expr')

  return (Type.fn <$> traverse typeOf paramdecls' <*> typeOf expr', paramdecls', decls', expr')

instance TypeAnnotatable AST.Program' where
  annotateType _ (AST.AProgram p languagedecl extensions decls) = do
    context' <- gets (withName "unit") >>= withDecls decls {-isTopLevel=-} True
    context'' <- foldM addExceptionDeclaration context' decls
    decls' <- withStateTAE (const context'') (mapM annotateTopDecl decls)

    t' <- case find isMain decls' of
      Just (AST.DeclFun (_, t'@(Just (Type (AST.TypeFun _ args _)))) _ _ _ _ _ _ _) | length args == 1 -> do
        return t'
      Just (AST.DeclFun (p', Just (Type (AST.TypeFun _ args _))) _ _ _ _ _ _ _) -> do
        let message = "main function must have exactly one parameter, got " ++ show (length args)
        tellD [diagnostic Error INCORRECT_ARITY_OF_MAIN (pointRange p') message]
        return Nothing
      Just (AST.DeclFun (_, Just t'@(Type _)) _ _ _ _ _ _ _) -> do
        error $ "unexpected main function type " ++ show t'
      Just (AST.DeclFun (_, Nothing) _ _ _ _ _ _ _) -> do
        return Nothing
      Just _ -> do
        error "isMain is true only on a DeclFun"
      Nothing -> do
        tellD [diagnostic Error MISSING_MAIN (pointRange p) "not found: main function"]
        return Nothing

    return (AST.AProgram (p, t') (stub languagedecl) (stubL extensions) decls')
    where
      addExceptionDeclaration context (AST.DeclExceptionType p' type_) =
        withStateTAE (const context) $ do
          type' <- sanitizeT type_
          case Context.withExceptionType type' context of
            Right context' -> pure context'
            Left issue -> do
              tellD [issue {range = pointRange p'}]
              pure context
      addExceptionDeclaration context _ = pure context

      annotateTopDecl f@(AST.DeclExceptionType {}) = pure $ stub f
      annotateTopDecl decl = inferType decl

      isMain (AST.DeclFun _ _ (AST.StellaIdent name) _ _ _ _ _) | name == "main" = True
      isMain _ = False

instance TypeAnnotatable AST.Decl' where
  annotateType _ (AST.DeclFun p annotations (AST.StellaIdent fname) paramdecls returntype throwtype decls expr) = do
    declaredType <- gets (Context.typeOf fname)
    (functionType, paramdecls', decls', expr') <- annotateFunction declaredType p annotations fname paramdecls returntype throwtype decls expr

    return
      ( AST.DeclFun
          (p, functionType)
          (stubL annotations)
          (AST.StellaIdent fname)
          paramdecls'
          (stub returntype)
          (stub throwtype)
          decls'
          expr'
      )
  annotateType _ (AST.DeclFunGeneric p annotations (AST.StellaIdent fname) parameters paramdecls returntype throwtype decls expr) = do
    current <- get
    let (resolved, context') = Context.bindTypeVariables parameters current
        declaredType = case Context.typeOf fname current of
          Just (Type (AST.TypeForAll () declaredParameters body))
            | length declaredParameters == length resolved ->
                Just $ Substitution.substitute (zip declaredParameters (fmap (Type . AST.TypeVar ()) resolved)) (Type body)
          _ -> Nothing
        declaredMetaVariables = maybe mempty (Unification.freeMetaVars $ Context.metaVars current) declaredType
        externalMetaVariables = Context.metaVars current `Set.difference` declaredMetaVariables
        rigidVariables = Set.fromList [name | AST.StellaIdent name <- resolved]
        applySubstitution substitution (functionType, paramdecls', decls', expr') =
          ( Substitution.apply substitution <$> (declaredType <|> functionType),
            fmap (fmap $ applyTypeAnnotation substitution) paramdecls',
            fmap (fmap $ applyTypeAnnotation substitution) decls',
            fmap (applyTypeAnnotation substitution) expr'
          )
    (functionType, paramdecls', decls', expr') <-
      solveScopedConstraints p externalMetaVariables rigidVariables DiscardEscaping applySubstitution $
        withStateTAE (const context') $
          annotateFunction declaredType p annotations fname paramdecls returntype throwtype decls expr
    let universalType = Type . AST.TypeForAll () resolved . Type.toAST <$> functionType
    case universalType of
      Just type_ -> modify (Context.withTyped fname type_)
      Nothing -> pure ()
    return
      ( AST.DeclFunGeneric
          (p, universalType)
          (stubL annotations)
          (AST.StellaIdent fname)
          resolved
          paramdecls'
          (stub returntype)
          (stub throwtype)
          decls'
          expr'
      )
  annotateType _ f@(AST.DeclTypeAlias {}) = do
    return $ stub f
  annotateType _ f@(AST.DeclExceptionType p t) = do
    t' <- sanitizeT t
    context <- get
    _ <- case Context.withExceptionType t' context of
      Right c -> put c
      Left issue -> tellD [issue {range = pointRange p}]
    return $ stub f
  annotateType _ f@(AST.DeclExceptionVariant p (AST.StellaIdent name) t) = do
    t' <- sanitizeT t
    context <- get
    _ <- case Context.withExceptionVariant name t' context of
      Right c -> put c
      Left issue -> tellD [issue {range = pointRange p}]
    return $ stub f

instance TypeAnnotatable AST.LocalDecl' where
  annotateType _ (AST.ALocalDecl p decl) = do
    decl' <- inferType decl
    let t' = typeOf decl'
    return (AST.ALocalDecl (p, t') decl')

instance TypeAnnotatable AST.ExprData' where
  annotateType t (AST.SomeExprData p expr) = do
    expr' <- annotateType t expr
    return (AST.SomeExprData (p, typeOf expr') expr')
  annotateType t (AST.NoExprData p) = do
    t' <- Just <$> liftType p AST.TypeUnit t
    return (AST.NoExprData (p, t'))

instance TypeAnnotatable AST.Expr' where
  annotateType t x@(AST.Sequence {}) = do
    annotateRefExprType t x annotateType
  annotateType t x@(AST.Assign {}) = do
    annotateRefExprType t x annotateType
  annotateType t (AST.If p condition thenB elseB) = do
    condition' <- checkType (Type.fromAST' AST.TypeBool) condition
    thenB' <- annotateType t thenB
    elseB' <- annotateType (typeOf thenB') elseB
    let t' = typeOf thenB' <|> typeOf elseB'
    return $ AST.If (p, t') condition' thenB' elseB'
  annotateType t (AST.Let p bindings inExpr) =
    annotateLetType t p bindings inExpr annotateType
  annotateType _ x@(AST.LetRec {}) = do
    tellD [notImplemented (annotation x) "LetRec"]
    return $ stub x
  annotateType expected (AST.TypeAbstraction p parameters expr) = do
    parameters' <- validateTypeParameters p parameters
    context <- get
    let (resolved, context') = Context.bindTypeVariables parameters' context
        externalMetaVariables = Context.metaVars context
        rigidVariables = Set.fromList [name | AST.StellaIdent name <- resolved]
        applySubstitution substitution = fmap $ applyTypeAnnotation substitution
    expr' <-
      solveScopedConstraints p externalMetaVariables rigidVariables RejectEscaping applySubstitution $
        withStateTAE (const context') (inferType expr)
    actual <- case typeOf expr' of
      Just body -> Just <$> liftType' p (Type $ AST.TypeForAll () resolved (Type.toAST body)) expected
      Nothing -> pure Nothing
    return $ AST.TypeAbstraction (p, actual) resolved expr'
  annotateType t (AST.LessThan p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2B annotateType annotateType t lhs rhs
    return (AST.LessThan (p, t') lhs' rhs')
  annotateType t (AST.LessThanOrEqual p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2B annotateType annotateType t lhs rhs
    return (AST.LessThanOrEqual (p, t') lhs' rhs')
  annotateType t (AST.GreaterThan p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2B annotateType annotateType t lhs rhs
    return (AST.GreaterThan (p, t') lhs' rhs')
  annotateType t (AST.GreaterThanOrEqual p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2B annotateType annotateType t lhs rhs
    return (AST.GreaterThanOrEqual (p, t') lhs' rhs')
  annotateType t (AST.Equal p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2B annotateType annotateType t lhs rhs
    return (AST.Equal (p, t') lhs' rhs')
  annotateType t (AST.NotEqual p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2B annotateType annotateType t lhs rhs
    return (AST.NotEqual (p, t') lhs' rhs')
  annotateType t (AST.TypeAsc p expr type_) = do
    type'' <- sanitizeT type_
    expr' <- checkType type'' expr
    t' <- liftType' p type'' t
    return (AST.TypeAsc (p, Just t') expr' (stub type_))
  annotateType t (AST.TypeCast p expr type_) = do
    type'' <- sanitizeT type_
    expr' <- inferType expr
    t' <- liftType' p type'' t
    return (AST.TypeCast (p, Just t') expr' (stub type_))
  annotateType t (AST.Abstraction p paramdecls expr) =
    annotateAbstractionType t p paramdecls expr annotateType
  annotateType t (AST.Variant p (AST.StellaIdent tag) expr) = do
    exprTyping <- variantFieldTyping p tag t
    exprType <- variantExprTyping exprTyping expr
    expr' <- annotateType exprType expr
    let t' = exprType >> t
    return (AST.Variant (p, t') (AST.StellaIdent tag) expr')
  annotateType t (AST.Match p expr cases) =
    annotateMatchType t p expr cases annotateType
  annotateType Nothing (AST.List p []) = do
    isBottom <- isAvailable Extension.AmbiguousTypeAsBottom
    unless isBottom $ do
      let message = "type inference for empty lists is not supported (use type ascriptions)"
      tellD [diagnostic Error AMBIGUOUS_LIST_TYPE (pointRange p) message]

    let t = if isBottom then Just $ list $ Type.fromAST' AST.TypeBottom else Nothing
    return (AST.List (p, t) [])
  annotateType (Just t) (AST.List p []) = do
    itemT <- listItemType p Expected (Just t)
    return (AST.List (p, itemT >> Just t) [])
  annotateType t (AST.List p (x : xs)) = do
    let expectedListType = case t of
          Just (Type (AST.TypeTop ())) -> Nothing
          _ -> t
    itemT <- listItemType p Expected expectedListType

    x' <- annotateType itemT x
    let t' = fmap Type.list (itemT <|> typeOf x')

    xs'' <- annotateType t' (AST.List p xs)
    let xs' = case xs'' of
          (AST.List (_, _) xs''') -> xs'''
          _ -> error "type annotation changed an AST"

    return (AST.List (p, t') (x' : xs'))
  annotateType t (AST.Add p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2T annotateType annotateType t lhs rhs
    return (AST.Add (p, t') lhs' rhs')
  annotateType t (AST.Subtract p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2T annotateType annotateType t lhs rhs
    return (AST.Subtract (p, t') lhs' rhs')
  annotateType t (AST.LogicOr p lhs rhs) = do
    lhs' <- checkType (Type.fromAST' AST.TypeBool) lhs
    rhs' <- checkType (Type.fromAST' AST.TypeBool) rhs
    t' <- liftType p AST.TypeBool t
    return (AST.LogicOr (p, Just t') lhs' rhs')
  annotateType t (AST.Multiply p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2T annotateType annotateType t lhs rhs
    return (AST.Multiply (p, t') lhs' rhs')
  annotateType t (AST.Divide p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2T annotateType annotateType t lhs rhs
    return (AST.Divide (p, t') lhs' rhs')
  annotateType t (AST.LogicAnd p lhs rhs) = do
    lhs' <- checkType (Type.fromAST' AST.TypeBool) lhs
    rhs' <- checkType (Type.fromAST' AST.TypeBool) rhs
    t' <- liftType p AST.TypeBool t
    return (AST.LogicAnd (p, Just t') lhs' rhs')
  annotateType t x@(AST.Ref {}) =
    annotateRefExprType t x annotateType
  annotateType t x@(AST.Deref {}) = do
    annotateRefExprType t x annotateType
  annotateType t (AST.Application p f xs) =
    annotateApplicationType t p f xs annotateType
  annotateType expected (AST.TypeApplication p expr types) = do
    expr' <- inferType expr
    types' <- mapM sanitizeT types
    result <- case typeOf expr' of
      Just (Type (AST.TypeForAll () parameters body))
        | length parameters == length types' -> do
            let instantiated = Substitution.substitute (zip parameters types') (Type body)
            Just <$> liftType' p instantiated expected
        | otherwise -> do
            let message =
                  "expected " ++ show (length parameters) ++ " type arguments, got " ++ show (length types')
            tellD [diagnostic Error INCORRECT_NUMBER_OF_TYPE_ARGUMENTS (pointRange p) message]
            return Nothing
      Just actual -> do
        let message = "expected a generic function, got " ++ show actual
        tellD [diagnostic Error NOT_A_GENERIC_FUNCTION (pointRange $ positionOf expr') message]
        return Nothing
      Nothing -> return Nothing
    return $ AST.TypeApplication (p, result) expr' (fmap stub types)
  annotateType t (AST.DotRecord p expr (AST.StellaIdent field)) =
    annotateDotRecordType t p expr field annotateType
  annotateType t (AST.DotTuple p expr index) =
    annotateDotTupleType t p expr index annotateType
  annotateType t (AST.Tuple p exprs) =
    annotateTupleType t p exprs annotateType
  annotateType t (AST.Record p bindings) =
    annotateRecordType t p bindings annotateType
  annotateType t (AST.ConsList p head'' tail'') = do
    let expectedListType = case t of
          Just (Type (AST.TypeTop ())) -> Nothing
          _ -> t
    headT <- listItemType p Expected expectedListType

    head' <- annotateType headT head''
    let itemT = headT <|> snd (annotation head')
        listT = fmap Type.list itemT

    tail' <- case tail'' of
      AST.List {} -> annotateType listT tail''
      AST.ConsList {} -> annotateType listT tail''
      AST.Tail {} -> annotateType listT tail''
      _ -> do
        inferred <- inferType tail''
        case typeOf inferred of
          Just actual -> do
            isSubtyping <- isAvailable Extension.StructuralSubtyping
            if isSubtyping
              then do
                _ <- liftType' (annotation tail'') actual listT
                pure ()
              else do
                let toDiagnostic expected actual' =
                      mismatchSS UNEXPECTED_TYPE_FOR_EXPRESSION (annotation tail'') (show expected) (show actual')
                _ <- ensureEqType (annotation tail'') toDiagnostic actual listT
                pure ()
            pure ()
          Nothing -> pure ()
        pure inferred
    let tailT = listT <|> snd (annotation tail')

    t' <- fmap Type.list <$> listItemType (annotation tail'') Inferred tailT
    return (AST.ConsList (p, t') head' tail')
  annotateType t (AST.Head p expr) = do
    let listT = fmap Type.list t
    expr' <- annotateType listT expr

    let listT' = listT <|> snd (annotation expr')
    t' <- listItemType (annotation expr) Inferred listT'

    return (AST.Head (p, t') expr')
  annotateType t (AST.IsEmpty p expr) = do
    t' <- liftType p AST.TypeBool t
    expr' <- inferType expr
    _ <- uncurry (`listItemType` Inferred) $ annotation expr'
    return (AST.IsEmpty (p, Just t') expr')
  annotateType t (AST.Tail p expr) = do
    isTypeReconstruction <- isAvailable Extension.TypeReconstruction
    case (isTypeReconstruction, t) of
      (False, Just (Type (AST.TypeList _ item))) -> do
        let listT = Just $ Type.list $ Type item
        expr' <- annotateType listT expr
        return (AST.Tail (p, listT <|> typeOf expr') expr')
      _ -> do
        expr' <- inferType expr
        itemT <- listItemType (annotation expr) Inferred (typeOf expr')
        t' <- traverse (\item -> liftType' p (Type.list item) t) itemT
        return (AST.Tail (p, t') expr')
  annotateType t x@(AST.Panic {}) =
    annotateExceptionExprType t x annotateType
  annotateType t x@(AST.Throw {}) = do
    annotateExceptionExprType t x annotateType
  annotateType t x@(AST.TryCatch {}) = do
    annotateExceptionExprType t x annotateType
  annotateType t x@(AST.TryWith {}) = do
    annotateExceptionExprType t x annotateType
  annotateType t x@(AST.TryCastAs {}) = do
    annotateExceptionExprType t x annotateType
  annotateType t x@(AST.Inl {}) = do
    annotateSumExprType t x annotateType
  annotateType t x@(AST.Inr {}) = do
    annotateSumExprType t x annotateType
  annotateType t (AST.Succ p expr) = do
    expr' <- checkType (Type.fromAST' AST.TypeNat) expr
    t' <- liftType p AST.TypeNat t
    return $ AST.Succ (p, Just t') expr'
  annotateType t (AST.LogicNot p expr) = do
    expr' <- checkType (Type.fromAST' AST.TypeBool) expr
    t' <- liftType p AST.TypeBool t
    return $ AST.LogicNot (p, Just t') expr'
  annotateType t (AST.Pred p expr) = do
    expr' <- checkType (Type.fromAST' AST.TypeNat) expr
    t' <- liftType p AST.TypeNat t
    return $ AST.Succ (p, Just t') expr'
  annotateType t (AST.IsZero p expr) = do
    expr' <- checkType (Type.fromAST' AST.TypeNat) expr
    t' <- liftType p AST.TypeBool t
    return $ AST.IsZero (p, Just t') expr'
  annotateType Nothing (AST.Fix p expr) = do
    expr' <- inferType expr
    metaVariables <- gets Context.metaVars
    t' <- case typeOf expr' of
      Just (Type (AST.TypeFun () [arg] ret)) | Unification.alphaEq (Type arg) (Type ret) -> return $ Just (Type ret)
      Just (Type (AST.TypeFun () [arg] ret))
        | not $ null (Unification.freeMetaVars metaVariables (Type arg) <> Unification.freeMetaVars metaVariables (Type ret)) -> do
            tellC [Constraint.Eq p (Type arg) (Type ret)]
            return $ Just (Type ret)
      Just t@(Type (AST.TypeFun () [_] _)) -> do
        tellD [mismatchSS UNEXPECTED_TYPE_FOR_EXPRESSION p "T -> T" (show t)]
        return Nothing
      Just t@(Type (AST.TypeFun () _ _)) -> do
        tellD [mismatchSS INCORRECT_NUMBER_OF_ARGUMENTS p "T -> T" (show t)]
        return Nothing
      Just t -> do
        tellD [mismatchSS NOT_A_FUNCTION p "T -> T" (show t)]
        return Nothing
      Nothing -> return Nothing
    return (AST.Fix (p, t') expr')
  annotateType (Just t) (AST.Fix p expr) = do
    let f = Type.fn [t] t
    expr' <- checkType f expr
    let t' = typeOf expr'
    return (AST.Fix (p, t') expr')
  annotateType t (AST.NatRec p n z s) = do
    n' <- checkType (Type.fromAST' AST.TypeNat) n
    z' <- annotateType t z

    let s't = (\(Type x) -> Type $ AST.TypeFun () [AST.TypeNat ()] (AST.TypeFun () [x] x)) <$> typeOf z'
    s' <- annotateType s't s

    let t' = typeOf z'
    return $ AST.NatRec (p, t') n' z' s'
  annotateType _ x@(AST.Fold {}) = do
    tellD [notImplemented (annotation x) "Fold"]
    return $ stub x
  annotateType _ x@(AST.Unfold {}) = do
    tellD [notImplemented (annotation x) "Unfold"]
    return $ stub x
  annotateType t (AST.ConstTrue p) = do
    t' <- liftType p AST.TypeBool t
    return $ AST.ConstTrue (p, Just t')
  annotateType t (AST.ConstFalse p) = do
    t' <- liftType p AST.TypeBool t
    return $ AST.ConstFalse (p, Just t')
  annotateType t (AST.ConstUnit p) = do
    t' <- liftType p AST.TypeUnit t
    return (AST.ConstUnit (p, Just t'))
  annotateType t (AST.ConstInt p n) = do
    t' <-
      if 0 <= n
        then
          Just <$> liftType p AST.TypeNat t
        else do
          tellD [notImplemented p "Negative Integer"]
          return Nothing

    return $ AST.ConstInt (p, t') n
  annotateType t x@(AST.ConstMemory {}) =
    annotateRefExprType t x annotateType
  annotateType t (AST.Var p stellaident@(AST.StellaIdent name)) = do
    context <- get

    t' <- case Context.typeOf name context of
      (Just t'') -> do
        Just <$> liftType' p t'' t
      Nothing -> do
        tellD [Context.unknownName p name]
        return Nothing

    return $ AST.Var (p, t') stellaident

stub :: (Functor f) => f Position -> f (Position, Maybe Type)
stub = fmap (,Nothing)

stubL :: (Functor f) => [f Position] -> [f (Position, Maybe Type)]
stubL = fmap (fmap (,Nothing))
