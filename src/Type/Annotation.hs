{-# LANGUAGE TupleSections #-}

module Type.Annotation (annotateType, inferType) where

import Annotation (annotation)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (unless, void)
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (find)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (Error), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Application (annotateAbstractionType, annotateApplicationType)
import qualified Type.Context as Context
import Type.Core (Type (Type))
import qualified Type.Core as Type
import Type.Decl (toPair, withDecls, withParamDecls)
import Type.Env (TypeAnnotationEnv, typeOf, withStateTAE)
import Type.Expectation (TypeKind (Expected, Inferred), liftType, liftType', listItemType, mismatchSS, sanitizeT)
import Type.Expression (annotateTT2B, annotateTT2T)
import Type.Match (annotateMatchType)
import Type.Record (annotateDotRecordType, annotateRecordType)
import Type.Tuple (annotateDotTupleType, annotateTupleType)
import Type.Variant (variantExprTyping, variantFieldTyping)

class TypeAnnotatable f where
  annotateType :: Maybe Type -> f Position -> TypeAnnotationEnv (f (Position, Maybe Type))

checkType :: (TypeAnnotatable f) => Type -> f Position -> TypeAnnotationEnv (f (Position, Maybe Type))
checkType t = annotateType $ Just t

inferType :: (TypeAnnotatable f) => f Position -> TypeAnnotationEnv (f (Position, Maybe Type))
inferType = annotateType Nothing

instance TypeAnnotatable AST.Program' where
  annotateType _ (AST.AProgram p languagedecl extensions decls) = do
    context' <- get >>= withDecls decls
    decls' <- withStateTAE (const context') (mapM inferType decls)

    t' <- case find isMain decls' of
      Just (AST.DeclFun (_, t'@(Just (Type (AST.TypeFun _ args _)))) _ _ _ _ _ _ _) | length args == 1 -> do
        return t'
      Just (AST.DeclFun (p', Just (Type (AST.TypeFun _ args _))) _ _ _ _ _ _ _) -> do
        let message = "main function must have exactly one parameter, got " ++ show (length args)
        tell [diagnostic Error INCORRECT_ARITY_OF_MAIN (pointRange p') message]
        return Nothing
      Just (AST.DeclFun (_, Just t'@(Type _)) _ _ _ _ _ _ _) -> do
        error $ "unexpected main function type " ++ show t'
      Just (AST.DeclFun (_, Nothing) _ _ _ _ _ _ _) -> do
        return Nothing
      Just _ -> do
        error "isMain is true only on a DeclFun"
      Nothing -> do
        tell [diagnostic Error MISSING_MAIN (pointRange p) "not found: main function"]
        return Nothing

    return (AST.AProgram (p, t') (stub languagedecl) (stubL extensions) decls')
    where
      isMain (AST.DeclFun _ _ (AST.StellaIdent name) _ _ _ _ _) | name == "main" = True
      isMain _ = False

instance TypeAnnotatable AST.Decl' where
  annotateType _ (AST.DeclFun p annotations stellaident paramdecls returntype throwtype decls expr) = do
    unless (null annotations) $ tell [notImplemented p "DeclFun annotations"]

    context' <- get >>= withDecls decls >>= withParamDecls paramdecls

    () <- case throwtype of
      (AST.NoThrowType _) -> pure ()
      (AST.SomeThrowType _ _) -> tell [notImplemented p "DeclFun ThrowType"]

    decls' <- withStateTAE (const context') (mapM inferType decls)
    expr' <- withStateTAE (const context') (annotateType (Type.fromReturn returntype) expr)

    argTypes <- mapM toPair paramdecls

    let t' = fmap (Type.fn $ fmap snd argTypes) (typeOf expr')

    return
      ( AST.DeclFun
          (p, t')
          (stubL annotations)
          stellaident
          (stubL paramdecls)
          (stub returntype)
          (stub throwtype)
          decls'
          expr'
      )
  annotateType _ f@(AST.DeclFunGeneric {}) = do
    return $ stub f
  annotateType _ f@(AST.DeclTypeAlias {}) = do
    return $ stub f
  annotateType _ f@(AST.DeclExceptionType {}) = do
    return $ stub f
  annotateType _ f@(AST.DeclExceptionVariant {}) = do
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
  annotateType _ x@(AST.Sequence p _ _) = do
    tell [notImplemented p "Sequence"]
    return $ stub x
  annotateType _ x@(AST.Assign {}) = do
    tell [notImplemented (annotation x) "Assign"]
    return $ stub x
  annotateType t (AST.If p condition thenB elseB) = do
    condition' <- checkType (Type.fromAST' AST.TypeBool) condition
    thenB' <- annotateType t thenB
    elseB' <- annotateType (typeOf thenB') elseB
    let t' = typeOf thenB' <|> typeOf elseB'
    return $ AST.If (p, t') condition' thenB' elseB'
  annotateType t (AST.Let p [AST.APatternBinding p' (AST.PatternVar p'' (AST.StellaIdent name)) expr] inExpr) = do
    expr' <- inferType expr
    let expr't = typeOf expr'

    inExpr' <- case expr't of
      (Just t') -> do
        context <- gets (Context.withTyped name t')
        withStateTAE (const context) (annotateType t inExpr)
      Nothing -> pure $ stub inExpr

    let t' = typeOf inExpr'
        pattern' = AST.PatternVar (p'', Nothing) (AST.StellaIdent name)
        binding' = AST.APatternBinding (p', Nothing) pattern' expr'

    return $ AST.Let (p, t') [binding'] inExpr'
  annotateType _ x@(AST.Let p [_] _) = do
    tell [notImplemented p "LetIn StructuralPattern"]
    return $ stub x
  annotateType _ x@(AST.Let p _ _) = do
    tell [notImplemented p "LetManyBindings"]
    return $ stub x
  annotateType _ x@(AST.LetRec {}) = do
    tell [notImplemented (annotation x) "LetRec"]
    return $ stub x
  annotateType _ x@(AST.TypeAbstraction {}) = do
    tell [notImplemented (annotation x) "TypeAbstraction"]
    return $ stub x
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
    type'' <- Type . void <$> sanitizeT type_
    expr' <- checkType type'' expr
    t' <- liftType' p type'' t
    return (AST.TypeAsc (p, Just t') expr' (stub type_))
  annotateType _ x@(AST.TypeCast {}) = do
    tell [notImplemented (annotation x) "TypeCast"]
    return $ stub x
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
    let message = "type inference for empty lists is not supported (use type ascriptions)"
    tell [diagnostic Error AMBIGUOUS_LIST_TYPE (pointRange p) message]
    return (AST.List (p, Nothing) [])
  annotateType (Just t) (AST.List p []) = do
    itemT <- listItemType p Expected (Just t)
    return (AST.List (p, itemT >> Just t) [])
  annotateType t (AST.List p (x : xs)) = do
    itemT <- listItemType p Expected t

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
  annotateType _ x@(AST.LogicOr {}) = do
    tell [notImplemented (annotation x) "LogicOr"]
    return $ stub x
  annotateType t (AST.Multiply p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2T annotateType annotateType t lhs rhs
    return (AST.Multiply (p, t') lhs' rhs')
  annotateType t (AST.Divide p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2T annotateType annotateType t lhs rhs
    return (AST.Divide (p, t') lhs' rhs')
  annotateType _ x@(AST.LogicAnd {}) = do
    tell [notImplemented (annotation x) "LogicAnd"]
    return $ stub x
  annotateType _ x@(AST.Ref {}) = do
    tell [notImplemented (annotation x) "Ref"]
    return $ stub x
  annotateType _ x@(AST.Deref {}) = do
    tell [notImplemented (annotation x) "Deref"]
    return $ stub x
  annotateType t (AST.Application p f xs) =
    annotateApplicationType t p f xs annotateType
  annotateType _ x@(AST.TypeApplication {}) = do
    tell [notImplemented (annotation x) "TypeApplication"]
    return $ stub x
  annotateType t (AST.DotRecord p expr (AST.StellaIdent field)) =
    annotateDotRecordType t p expr field annotateType
  annotateType t (AST.DotTuple p expr index) =
    annotateDotTupleType t p expr index annotateType
  annotateType t (AST.Tuple p exprs) =
    annotateTupleType t p exprs annotateType
  annotateType t (AST.Record p bindings) =
    annotateRecordType t p bindings annotateType
  annotateType t (AST.ConsList p head'' tail'') = do
    headT <- listItemType p Expected t

    head' <- annotateType headT head''
    let itemT = headT <|> snd (annotation head')
        listT = fmap Type.list itemT

    tail' <- annotateType listT tail''
    let tailT = listT <|> snd (annotation tail')

    t' <- listItemType (annotation tail'') Inferred tailT
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
    headT <- listItemType p Expected t
    let listT = fmap Type.list headT
    expr' <- annotateType listT expr
    return (AST.Tail (p, listT) expr')
  annotateType _ x@(AST.Panic {}) = do
    tell [notImplemented (annotation x) "Panic"]
    return $ stub x
  annotateType _ x@(AST.Throw {}) = do
    tell [notImplemented (annotation x) "Throw"]
    return $ stub x
  annotateType _ x@(AST.TryCatch {}) = do
    tell [notImplemented (annotation x) "TryCatch"]
    return $ stub x
  annotateType _ x@(AST.TryWith {}) = do
    tell [notImplemented (annotation x) "TryWith"]
    return $ stub x
  annotateType _ x@(AST.TryCastAs {}) = do
    tell [notImplemented (annotation x) "TryCastAs"]
    return $ stub x
  annotateType Nothing (AST.Inl p expr) = do
    expr' <- inferType expr -- TODO: make a function for each diagnostic
    let message = "type inference for sum types is not supported (use type ascriptions)"
     in tell [diagnostic Error AMBIGUOUS_SUM_TYPE (pointRange p) message]
    return (AST.Inl (p, Nothing) expr')
  annotateType (Just (Type (AST.TypeSum _ inl inr))) (AST.Inl p expr) = do
    expr' <- checkType (Type inl) expr
    let t' = (\(Type x) -> Type (AST.TypeSum () x inr)) <$> typeOf expr'
    return (AST.Inl (p, t') expr')
  annotateType (Just t) (AST.Inl p expr) = do
    expr' <- inferType expr
    let expr't = maybe "?" show $ typeOf expr'
        message = "expected " ++ show t ++ ", but got inl(" ++ expr't ++ ")"
     in tell [diagnostic Error UNEXPECTED_INJECTION (pointRange p) message]
    return (AST.Inl (p, Nothing) expr')
  annotateType Nothing (AST.Inr p expr) = do
    expr' <- inferType expr
    let message = "type inference for sum types is not supported (use type ascriptions)"
     in tell [diagnostic Error AMBIGUOUS_SUM_TYPE (pointRange p) message]
    return (AST.Inr (p, Nothing) expr')
  annotateType (Just (Type (AST.TypeSum _ inl inr))) (AST.Inr p expr) = do
    expr' <- checkType (Type inr) expr
    let t' = (\(Type x) -> Type (AST.TypeSum () inl x)) <$> typeOf expr'
    return (AST.Inr (p, t') expr')
  annotateType (Just t) (AST.Inr p expr) = do
    expr' <- inferType expr
    let expr't = maybe "?" show $ typeOf expr'
        message = "expected " ++ show t ++ ", but got inr(" ++ expr't ++ ")"
     in tell [diagnostic Error UNEXPECTED_INJECTION (pointRange p) message]
    return (AST.Inr (p, Nothing) expr')
  annotateType t (AST.Succ p expr) = do
    expr' <- checkType (Type.fromAST' AST.TypeNat) expr
    t' <- liftType p AST.TypeNat t
    return $ AST.Succ (p, Just t') expr'
  annotateType _ x@(AST.LogicNot {}) = do
    tell [notImplemented (annotation x) "LogicNot"]
    return $ stub x
  annotateType _ x@(AST.Pred {}) = do
    tell [notImplemented (annotation x) "Pred"]
    return $ stub x
  annotateType t (AST.IsZero p expr) = do
    expr' <- checkType (Type.fromAST' AST.TypeNat) expr
    t' <- liftType p AST.TypeBool t
    return $ AST.IsZero (p, Just t') expr'
  annotateType Nothing (AST.Fix p expr) = do
    expr' <- inferType expr
    t' <- case typeOf expr' of
      Just (Type (AST.TypeFun () [arg] ret)) | arg == ret -> return $ Just (Type ret)
      Just t -> do
        tell [mismatchSS UNEXPECTED_TYPE_FOR_EXPRESSION p "T -> T" (show t)]
        return Nothing
      Nothing -> return Nothing
    return (AST.Fix (p, t') expr')
  annotateType (Just t) (AST.Fix p expr) = do
    let f = Type.fn [t] t
    expr' <- checkType (Type.fn [f] f) expr
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
    tell [notImplemented (annotation x) "Fold"]
    return $ stub x
  annotateType _ x@(AST.Unfold {}) = do
    tell [notImplemented (annotation x) "Unfold"]
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
          tell [notImplemented p "Negative Integer"]
          return Nothing

    return $ AST.ConstInt (p, t') n
  annotateType _ x@(AST.ConstMemory {}) = do
    tell [notImplemented (annotation x) "ConstMemory"]
    return $ stub x
  annotateType t (AST.Var p stellaident@(AST.StellaIdent name)) = do
    context <- get

    t' <- case Context.typeOf name context of
      (Just t'') -> do
        Just <$> liftType' p t'' t
      Nothing -> do
        tell [Context.unknownName p name]
        return Nothing

    return $ AST.Var (p, t') stellaident

stub :: (Functor f) => f Position -> f (Position, Maybe Type)
stub = fmap (,Nothing)

stubL :: (Functor f) => [f Position] -> [f (Position, Maybe Type)]
stubL = fmap (fmap (,Nothing))
