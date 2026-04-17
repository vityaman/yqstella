{-# LANGUAGE TupleSections #-}

module Type.Annotation (annotateType, inferType) where

import Annotation (annotation)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard, unless, void, when, zipWithM)
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Bifunctor
import Data.Foldable (find)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (Error), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import Misc.Duplicate (sepUniqDupBy)
import SyntaxGen.AbsStella (Binding')
import qualified SyntaxGen.AbsStella as AST
import qualified Type.Context as Context
import Type.Core (Type (Type))
import qualified Type.Core as Type
import Type.Decl (toPair, withDecls, withParamDecls)
import Type.Env (TypeAnnotationEnv, typeOf, withStateTAE)
import Type.Expectation (commonType, liftType, liftType', listItemType, mismatch, mismatchSS, sanitizeT)
import Type.ExprBinary (annotateTT2B, annotateTT2T)
import qualified Type.PatternMatching as PatternMatching

class TypeAnnotatable f where
  annotateType :: Maybe Type -> f Position -> TypeAnnotationEnv (f (Position, Maybe Type))

checkType :: (TypeAnnotatable f) => Type -> f Position -> TypeAnnotationEnv (f (Position, Maybe Type))
checkType t = annotateType $ Just t

inferType :: (TypeAnnotatable f) => f Position -> TypeAnnotationEnv (f (Position, Maybe Type))
inferType = annotateType Nothing

instance TypeAnnotatable AST.Program' where
  annotateType _ (AST.AProgram p languagedecl extensions decls) = do
    let languagedecl' = fmap (,Nothing) languagedecl
        extensions' = fmap (fmap (,Nothing)) extensions

    context' <- get >>= withDecls decls
    decls' <- withStateTAE (const context') (mapM inferType decls)

    let main = find isMain decls'
        type_ = main >>= declFunType'

    when (null main) $ tell [diagnostic Error MISSING_MAIN (pointRange p) "not found: main function"]

    () <- case (main, type_) of
      (Just (AST.DeclFun (p', _) _ _ _ _ _ _ _), Just (Type (AST.TypeFun _ args _))) | length args /= 1 -> do
        let message = "main function must have exactly one parameter, got " ++ show (length args)
        tell [diagnostic Error INCORRECT_ARITY_OF_MAIN (pointRange p') message]
      _ ->
        return ()

    return (AST.AProgram (p, type_) languagedecl' extensions' decls')
    where
      isMain (AST.DeclFun _ _ (AST.StellaIdent name) _ _ _ _ _) | name == "main" = True
      isMain _ = False

      declFunType' (AST.DeclFun (_, type_') _ _ _ _ _ _ _) = type_'
      declFunType' _ = error "Main declaration must be a function"

instance TypeAnnotatable AST.Decl' where
  annotateType _ (AST.DeclFun p annotations stellaident paramdecls returntype throwtype decls expr) = do
    unless (null annotations) $ tell [notImplemented p "DeclFun annotations"]

    let annotations' = fmap (fmap (,Nothing)) annotations
        paramdecls' = fmap (fmap (,Nothing)) paramdecls
        returntype' = fmap (,Nothing) returntype
        throwtype' = fmap (,Nothing) throwtype

    context' <- get >>= withDecls decls >>= withParamDecls paramdecls

    _ <- case throwtype of
      (AST.NoThrowType _) -> pure ()
      (AST.SomeThrowType _ _) -> tell [notImplemented p "DeclFun ThrowType"]

    let returntype'' = case returntype' of
          (AST.SomeReturnType _ t) -> Just $ Type.fromAST t
          (AST.NoReturnType _) -> Nothing

    decls' <- withStateTAE (const context') (mapM inferType decls)
    expr' <- withStateTAE (const context') (annotateType returntype'' expr)

    argTypes <- mapM toPair paramdecls

    let actualType = snd $ annotation expr'
        type' = fmap (Type.fn $ fmap snd argTypes) actualType

    return (AST.DeclFun (p, type') annotations' stellaident paramdecls' returntype' throwtype' decls' expr')
  annotateType _ f@(AST.DeclFunGeneric {}) = do
    return $ fmap (,Nothing) f
  annotateType _ f@(AST.DeclTypeAlias {}) = do
    return $ fmap (,Nothing) f
  annotateType _ f@(AST.DeclExceptionType {}) = do
    return $ fmap (,Nothing) f
  annotateType _ f@(AST.DeclExceptionVariant {}) = do
    return $ fmap (,Nothing) f

instance TypeAnnotatable AST.LocalDecl' where
  annotateType _ (AST.ALocalDecl p decl) = do
    decl' <- inferType decl
    let type' = snd $ annotation decl'
    return (AST.ALocalDecl (p, type') decl')

instance TypeAnnotatable AST.Expr' where
  annotateType _ x@(AST.Sequence p _ _) = do
    tell [notImplemented p "Sequence"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Assign {}) = do
    tell [notImplemented (annotation x) "Assign"]
    return $ fmap (,Nothing) x
  annotateType t (AST.If p condition thenB elseB) = do
    condition' <- checkType (Type.fromAST' AST.TypeBool) condition
    thenB' <- annotateType t thenB
    elseB' <- annotateType (snd $ annotation thenB') elseB

    let thenB'Type = snd $ annotation thenB'
        elseB'Type = snd $ annotation elseB'
        type' = thenB'Type <|> elseB'Type

    return $ AST.If (p, type') condition' thenB' elseB'
  annotateType t (AST.Let p [AST.APatternBinding p' (AST.PatternVar p'' (AST.StellaIdent name)) expr] inExpr) = do
    expr' <- inferType expr
    let expr't = snd $ annotation expr'

    context <- get
    let context' = fmap (\x -> Context.withTyped name x context) expr't

    inExpr' <- case context' of
      (Just context'') -> withStateTAE (const context'') (annotateType t inExpr)
      Nothing -> pure $ fmap (,Nothing) inExpr

    let t' = snd $ annotation inExpr'
        pattern' = AST.PatternVar (p'', Nothing) (AST.StellaIdent name)
        binding' = AST.APatternBinding (p', Nothing) pattern' expr'

    return $ AST.Let (p, t') [binding'] inExpr'
  annotateType _ x@(AST.Let p [_] _) = do
    tell [notImplemented p "LetIn StructuralPattern"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Let p _ _) = do
    tell [notImplemented p "LetManyBindings"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.LetRec {}) = do
    tell [notImplemented (annotation x) "LetRec"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.TypeAbstraction {}) = do
    tell [notImplemented (annotation x) "TypeAbstraction"]
    return $ fmap (,Nothing) x
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
  annotateType t (AST.TypeAsc p expr exprT) = do
    exprT'' <- Type . void <$> sanitizeT exprT
    expr' <- checkType exprT'' expr
    t' <- liftType' p exprT'' t
    let exprT' = fmap (,Nothing) exprT
    return (AST.TypeAsc (p, Just t') expr' exprT')
  annotateType _ x@(AST.TypeCast {}) = do
    tell [notImplemented (annotation x) "TypeCast"]
    return $ fmap (,Nothing) x
  annotateType t (AST.Abstraction p paramdecls expr) = do
    let paramdecls' = fmap (fmap (,Nothing)) paramdecls
    context' <- get >>= withParamDecls paramdecls

    let infer' = do
          expr' <- withStateTAE (const context') (inferType expr)
          argTypes <- mapM toPair paramdecls
          let returntype = snd $ annotation expr'
          return (fmap (Type.fn $ fmap snd argTypes) returntype, expr')

    (t', expr') <- case t of
      Just t'@(Type (AST.TypeFun () argtypes returntype)) -> do
        paramdecls'' <- mapM toPair paramdecls
        let actual = Data.Bifunctor.first annotation <$> zip paramdecls paramdecls''

        let expected = fmap Type.fromAST argtypes

            toDiagnostic ((p', (name, actual')), expected') = do
              guard $ actual' /= expected'
              let m = "(" ++ name ++ " : " ++ show actual' ++ ")"
              return $ mismatchSS UNEXPECTED_TYPE_FOR_PARAMETER p' (show expected') m

        tell $ mapMaybe toDiagnostic (zip actual expected)
        expr' <- withStateTAE (const context') (checkType (Type returntype) expr)

        return (Just t', expr')
      Just t'' -> do
        (t', expr') <- infer'
        tell [mismatchSS UNEXPECTED_LAMBDA p (show t'') (maybe "lambda" show t')]
        return (t', expr')
      Nothing ->
        infer'

    return $ AST.Abstraction (p, t') paramdecls' expr'
  annotateType t (AST.Variant p n@(AST.StellaIdent tag) expr) = do
    exprT <- case t of
      (Just t'@(Type (AST.TypeVariant () entries))) -> do
        let tag'' (AST.AVariantFieldType _ (AST.StellaIdent tag') _) = tag'
            entry = find (\x -> tag'' x == tag) entries
        case entry of
          Just (AST.AVariantFieldType _ _ t'') ->
            return $ Just t''
          Nothing -> do
            let message = "unexpected variant label " ++ tag ++ " for " ++ show t'
            tell [diagnostic Error UNEXPECTED_VARIANT_LABEL (pointRange p) message]
            return Nothing
      (Just t') -> do
        let message = "expected " ++ show t' ++ ", but got a variant"
        tell [diagnostic Error UNEXPECTED_VARIANT (pointRange p) message]
        return Nothing
      Nothing -> do
        let message = "type inference for variant types is not supported (use type ascriptions)"
        tell [diagnostic Error AMBIGUOUS_VARIANT_TYPE (pointRange p) message]
        return Nothing

    (expr', t'') <- case (expr, exprT) of
      (AST.NoExprData _, Just (AST.NoTyping _)) -> do
        return (fmap (,Nothing) expr, Just (Type.fromAST' AST.TypeUnit))
      (AST.NoExprData p', Just (AST.SomeTyping _ t')) -> do
        let message = "expected some variant label with type " ++ show (Type t') ++ ", got nullary"
        tell [diagnostic Error MISSING_DATA_FOR_LABEL (pointRange p') message]
        return (fmap (,Nothing) expr, Nothing)
      (AST.NoExprData _, Nothing) -> do
        return (fmap (,Nothing) expr, Nothing)
      (AST.SomeExprData p' expr', Just (AST.NoTyping _)) -> do
        expr'' <- inferType expr'
        let message = "expected nullary variant label"
        tell [diagnostic Error UNEXPECTED_DATA_FOR_NULLARY_LABEL (pointRange p') message]
        return (AST.SomeExprData (p', snd $ annotation expr'') expr'', Nothing)
      (AST.SomeExprData p' expr', Just (AST.SomeTyping _ t')) -> do
        expr'' <- checkType (Type.fromAST t') expr'
        return (AST.SomeExprData (p', snd $ annotation expr'') expr'', snd $ annotation expr'')
      (AST.SomeExprData p' expr', Nothing) -> do
        expr'' <- inferType expr'
        return (AST.SomeExprData (p', snd $ annotation expr'') expr'', Nothing)

    let t' = t'' >> t
    return (AST.Variant (p, t') n expr')
  annotateType _ (AST.Match p expr []) = do
    expr' <- inferType expr
    let message = "expected at least one match case"
     in tell [diagnostic Error ILLEGAL_EMPTY_MATCHING (pointRange p) message]
    return (AST.Match (p, Nothing) expr' [])
  annotateType t (AST.Match p expr cases) = do
    expr' <- inferType expr
    let expr't = snd $ annotation expr'

    cases' <- case expr't of
      Just t' -> mapM (annotateType' t') cases
      Nothing -> pure $ fmap (fmap (,Nothing)) cases

    () <- case (expr't, all (isJust . typeOf) cases') of
      (Just t', True)
        | not (PatternMatching.isExhaustive (patterns cases) t') ->
            let message = "nonexchaustive pattern-matching"
             in tell [diagnostic Error NONEXHAUSTIVE_MATCH_PATTERNS (pointRange p) message]
      _ -> pure ()

    t' <- commonType p (fmap annotation cases')

    return (AST.Match (p, t') expr' cases')
    where
      patterns = fmap (\(AST.AMatchCase _ x _) -> x)

      annotateType' patterntype (AST.AMatchCase p' pattern'' expr'') = do
        let pattern' = fmap (,Nothing) pattern''

        expr' <- case PatternMatching.bindings pattern'' patterntype of
          Right bindings' -> do
            context <- get
            let context' = foldr (uncurry Context.withTyped) context (Map.toList bindings')
            withStateTAE (const context') (annotateType t expr'')
          Left e -> do
            tell [e]
            return $ fmap (,Nothing) expr''

        let t' = snd $ annotation expr'

        return (AST.AMatchCase (p', t') pattern' expr')
  annotateType Nothing (AST.List p []) = do
    let message = "type inference for empty lists is not supported (use type ascriptions)"
    tell [diagnostic Error AMBIGUOUS_LIST_TYPE (pointRange p) message]
    return (AST.List (p, Nothing) [])
  annotateType (Just t) (AST.List p []) = do
    return (AST.List (p, Just t) [])
  annotateType t (AST.List p (x : xs)) = do
    headT <- case t of -- FIXME(vityaman): copy-pasted from Cons
      (Just (Type (AST.TypeList () itemT))) ->
        return $ Just $ Type itemT
      (Just t'') -> do
        -- FIXME(vityman): improve diagnostic
        let message = "expected " ++ show t'' ++ ", got list"
        tell [diagnostic Error UNEXPECTED_LIST (pointRange p) message]
        return Nothing
      Nothing ->
        return Nothing

    x' <- annotateType headT x
    let t' = fmap Type.list (headT <|> snd (annotation x'))

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
    return $ fmap (,Nothing) x
  annotateType t (AST.Multiply p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2T annotateType annotateType t lhs rhs
    return (AST.Multiply (p, t') lhs' rhs')
  annotateType t (AST.Divide p lhs rhs) = do
    (t', lhs', rhs') <- annotateTT2T annotateType annotateType t lhs rhs
    return (AST.Divide (p, t') lhs' rhs')
  annotateType _ x@(AST.LogicAnd {}) = do
    tell [notImplemented (annotation x) "LogicAnd"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Ref {}) = do
    tell [notImplemented (annotation x) "Ref"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Deref {}) = do
    tell [notImplemented (annotation x) "Deref"]
    return $ fmap (,Nothing) x
  annotateType t (AST.Application p f xs) = do
    f' <- inferType f
    let (f'Position, f'Type) = annotation f'

    (xs', type') <- case f'Type of
      Just (Type (AST.TypeFun _ argTypes returnType)) -> do
        let argtypes' = fmap Type argTypes
            returntype' = Type returnType

        xs' <- zipWithM annotateType (fmap Just argtypes') xs

        let expectedLen = length argtypes'
            actualLen = length xs

        returntype'' <-
          if expectedLen /= actualLen
            then do
              let message = "expected " ++ show expectedLen ++ " arguments, got " ++ show actualLen
              tell [diagnostic Error INCORRECT_NUMBER_OF_ARGUMENTS (pointRange p) message]
              return Nothing
            else
              return $ Just returntype'

        return (xs', returntype'')
      Just actual -> do
        xs' <- mapM inferType xs

        let unknown = Type.fromAST' AST.TypeAuto
            expectedArgTypes = fmap (fromMaybe unknown . typeOf) xs'
            expected = Type.fn expectedArgTypes unknown

        let message = "type mismatch: expected " ++ show expected ++ ", got " ++ show actual
        tell [diagnostic Error NOT_A_FUNCTION (pointRange f'Position) message]

        return (xs', Nothing)
      Nothing -> do
        xs' <- mapM inferType xs
        return (xs', Nothing)

    _ <- case (t, type') of
      (Just expected, Just actual)
        | expected /= actual ->
            tell [mismatch UNEXPECTED_TYPE_FOR_EXPRESSION p expected actual]
      _ -> return ()

    return $ AST.Application (p, t <|> type') f' xs'
  annotateType _ x@(AST.TypeApplication {}) = do
    tell [notImplemented (annotation x) "TypeApplication"]
    return $ fmap (,Nothing) x
  annotateType t (AST.DotRecord p expr (AST.StellaIdent field)) = do
    expr' <- inferType expr

    t' <- case snd $ annotation expr' of
      Just (Type (AST.TypeRecord _ fields)) -> do
        -- TODO(vityaman): make commons for records
        let toKV (AST.ARecordFieldType () (AST.StellaIdent k) v) = (k, Type v)
            tmap = Map.fromList $ fmap toKV fields

            t' = Map.lookup field tmap

        when (null t') $
          let message = "missing record field " ++ field ++ " : " ++ maybe "?" show t
           in tell [diagnostic Error UNEXPECTED_FIELD_ACCESS (pointRange p) message]

        return t'
      Just actual -> do
        let message =
              "type mismatch: expected record with "
                ++ (field ++ ": " ++ maybe "?" show t)
                ++ (", got " ++ show actual)
        tell [diagnostic Error NOT_A_RECORD (pointRange p) message]
        return Nothing
      Nothing ->
        return Nothing

    return (AST.DotRecord (p, t') expr' (AST.StellaIdent field))
  annotateType t (AST.DotTuple p expr index) = do
    expr' <- inferType expr

    t' <- case snd $ annotation expr' of
      _ | index == 0 -> do
        let message = "tuple index should be positive, got 0"
        tell [diagnostic Error TUPLE_INDEX_OUT_OF_BOUNDS (pointRange p) message]
        return Nothing
      Just actual@(Type (AST.TypeTuple _ ts)) | length ts < fromInteger index -> do
        let message =
              "type mismatch: expected tuple "
                ++ ("with size at least " ++ show index)
                ++ (", got " ++ show actual)
        tell [diagnostic Error TUPLE_INDEX_OUT_OF_BOUNDS (pointRange p) message]
        return Nothing
      Just (Type (AST.TypeTuple _ ts)) -> do
        let actual = ts !! fromInteger (index - 1)
        t' <- liftType p (const actual) t
        return $ Just t'
      Just actual -> do
        let message = "type mismatch: expected tuple, got " ++ show actual
        tell [diagnostic Error NOT_A_TUPLE (pointRange p) message]
        return Nothing
      Nothing ->
        return Nothing

    return (AST.DotTuple (p, t') expr' index)
  annotateType t (AST.Tuple p exprs) = do
    let untype (Type x) = x

    (exprs', isReliable) <- case t of
      Just (Type (AST.TypeTuple _ ts)) | length ts == length exprs -> do
        exprs' <- zipWithM checkType (fmap Type ts) exprs
        return (exprs', True)
      Just expected -> do
        exprs' <- mapM inferType exprs

        let code = case expected of
              (Type (AST.TypeTuple _ _)) -> UNEXPECTED_TUPLE_LENGTH
              _ -> UNEXPECTED_TUPLE

        let unknown = AST.TypeAuto ()
            actualTypes = fmap (maybe unknown untype . typeOf) exprs'
            actual = Type.fromAST $ AST.TypeTuple () actualTypes

        let message = "type mismatch: expected " ++ show expected ++ ", got " ++ show actual
        tell [diagnostic Error code (pointRange p) message]

        return (exprs', False)
      Nothing -> do
        exprs' <- mapM inferType exprs
        return (exprs', False)

    let t' = Type . AST.TypeTuple () <$> traverse (fmap untype . typeOf) exprs'

    return (AST.Tuple (p, if isReliable then t' else Nothing) exprs')
  annotateType t (AST.Record p bindings) = do
    let name (AST.ABinding _ (AST.StellaIdent name') _) = name'

        toDiagnostic (AST.ABinding p' (AST.StellaIdent name') _) =
          let message = "duplicate field: " ++ name'
           in diagnostic Error DUPLICATE_RECORD_FIELDS (pointRange p') message

        toMap :: Maybe Type -> Maybe (Map String Type)
        toMap (Just (Type (AST.TypeRecord () fields))) =
          let toKV (AST.ARecordFieldType () (AST.StellaIdent k) v) = (k, Type v)
           in Just $ Map.fromList $ fmap toKV fields
        toMap _ = Nothing

        toMap' :: [Binding' (a, Maybe Type)] -> Maybe (Map String Type)
        toMap' bindingsUniq = Map.fromList <$> traverse toKV bindingsUniq
          where
            toKV (AST.ABinding (_, t') (AST.StellaIdent name') _) = fmap (name',) t'

        lookup' :: String -> Maybe (Map String Type) -> Maybe Type
        lookup' _ Nothing = Nothing
        lookup' k (Just m) = Map.lookup k m

        annotateType' t'' (AST.ABinding p' name' expr) = do
          expr' <- annotateType t'' expr
          let t' = snd $ annotation expr'
          return (AST.ABinding (p', t') name' expr')

        annotateType'' tmap' x =
          annotateType' (lookup' (name x) tmap') x

    let (bindingsUniq, bindingsDup) = sepUniqDupBy name bindings
        expectedTMap = toMap t

    bindingsUniq' <- mapM (annotateType'' expectedTMap) bindingsUniq
    bindingsDup' <- mapM (annotateType'' expectedTMap) bindingsDup
    let bindings' = bindingsUniq' ++ bindingsDup'

    tell $ fmap toDiagnostic bindingsDup

    let actualTMap = toMap' bindingsUniq'

    t'' <- case (expectedTMap, actualTMap) of
      (Just expected, Just actual) -> do
        let missing = Map.keys $ Map.difference expected actual
        unless (null missing) $
          let message = "missing record fields: " ++ intercalate ", " missing
           in tell [diagnostic Error MISSING_RECORD_FIELDS (pointRange p) message]

        let unexpected = Map.keys $ Map.difference actual expected
        unless (null unexpected) $
          let message = "unexpected record fields: " ++ intercalate ", " missing
           in tell [diagnostic Error UNEXPECTED_RECORD_FIELDS (pointRange p) message]

        return $
          if null missing && null unexpected
            then Just expected
            else Nothing
      (Nothing, Just t') ->
        return $ Just t'
      (_, Nothing) ->
        return Nothing

    let toType map' =
          let xs = [AST.ARecordFieldType () (AST.StellaIdent k) v | (k, Type v) <- map']
           in Type $ AST.TypeRecord () xs

        t' = fmap (toType . Map.toList) t''

    return (AST.Record (p, t') bindings')
  annotateType t (AST.ConsList p head'' tail'') = do
    headT <- case t of
      (Just (Type (AST.TypeList () itemT))) ->
        return $ Just $ Type itemT
      (Just t'') -> do
        let message = "expected " ++ show t'' ++ ", got list"
        tell [diagnostic Error UNEXPECTED_LIST (pointRange p) message]
        return Nothing
      Nothing ->
        return Nothing

    head' <- annotateType headT head''
    let itemT = headT <|> snd (annotation head')
        listT = fmap Type.list itemT

    tail' <- annotateType listT tail''
    let tailT = listT <|> snd (annotation tail')

    t' <- listItemType (annotation tail'') tailT
    return (AST.ConsList (p, t') head' tail')
  annotateType t (AST.Head p expr) = do
    let listT = fmap Type.list t
    expr' <- annotateType listT expr

    let listT' = listT <|> snd (annotation expr')
    t' <- listItemType (annotation expr) listT'

    return (AST.Head (p, t') expr')
  annotateType t (AST.IsEmpty p expr) = do
    t' <- liftType p AST.TypeBool t
    expr' <- inferType expr
    _ <- uncurry listItemType $ annotation expr'
    return (AST.IsEmpty (p, Just t') expr')
  annotateType t (AST.Tail p expr) = do
    headT <- case t of -- FIXME(vityaman): copy-pasted from Cons
      (Just (Type (AST.TypeList () itemT))) ->
        return $ Just $ Type itemT
      (Just t'') -> do
        let message = "expected " ++ show t'' ++ ", got list"
        tell [diagnostic Error UNEXPECTED_LIST (pointRange p) message]
        return Nothing
      Nothing ->
        return Nothing

    let listT = fmap Type.list headT
    expr' <- annotateType listT expr

    return (AST.Tail (p, listT) expr')
  annotateType _ x@(AST.Panic {}) = do
    tell [notImplemented (annotation x) "Panic"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Throw {}) = do
    tell [notImplemented (annotation x) "Throw"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.TryCatch {}) = do
    tell [notImplemented (annotation x) "TryCatch"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.TryWith {}) = do
    tell [notImplemented (annotation x) "TryWith"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.TryCastAs {}) = do
    tell [notImplemented (annotation x) "TryCastAs"]
    return $ fmap (,Nothing) x
  annotateType Nothing (AST.Inl p expr) = do
    expr' <- inferType expr
    let message = "type inference for sum types is not supported (use type ascriptions)"
     in tell [diagnostic Error AMBIGUOUS_SUM_TYPE (pointRange p) message]
    return (AST.Inl (p, Nothing) expr')
  annotateType (Just (Type (AST.TypeSum _ inl inr))) (AST.Inl p expr) = do
    expr' <- checkType (Type inl) expr
    let t' = fmap (\(Type x) -> Type (AST.TypeSum () x inr)) $ snd $ annotation expr'
    return (AST.Inl (p, t') expr')
  annotateType (Just t) (AST.Inl p expr) = do
    expr' <- inferType expr
    let expr't = maybe "?" show $ snd $ annotation expr'
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
    let t' = fmap (\(Type x) -> Type (AST.TypeSum () inl x)) $ snd $ annotation expr'
    return (AST.Inr (p, t') expr')
  annotateType (Just t) (AST.Inr p expr) = do
    expr' <- inferType expr
    let expr't = maybe "?" show $ snd $ annotation expr'
        message = "expected " ++ show t ++ ", but got inr(" ++ expr't ++ ")"
     in tell [diagnostic Error UNEXPECTED_INJECTION (pointRange p) message]
    return (AST.Inr (p, Nothing) expr')
  annotateType t (AST.Succ p expr) = do
    expr' <- checkType (Type.fromAST' AST.TypeNat) expr
    t' <- liftType p AST.TypeNat t
    return $ AST.Succ (p, Just t') expr'
  annotateType _ x@(AST.LogicNot {}) = do
    tell [notImplemented (annotation x) "LogicNot"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Pred {}) = do
    tell [notImplemented (annotation x) "Pred"]
    return $ fmap (,Nothing) x
  annotateType t (AST.IsZero p expr) = do
    expr' <- checkType (Type.fromAST' AST.TypeNat) expr
    t' <- liftType p AST.TypeBool t
    return $ AST.IsZero (p, Just t') expr'
  annotateType Nothing (AST.Fix p expr) = do
    expr' <- inferType expr
    t' <- case snd $ annotation expr' of
      Just (Type (AST.TypeFun () [arg] ret)) | arg == ret -> return $ Just (Type ret)
      Just t -> do
        tell [mismatchSS UNEXPECTED_TYPE_FOR_EXPRESSION p "T -> T" (show t)]
        return Nothing
      Nothing -> return Nothing
    return (AST.Fix (p, t') expr')
  annotateType (Just t) (AST.Fix p expr) = do
    let f = Type.fn [t] t
    expr' <- checkType (Type.fn [f] f) expr
    let t' = snd $ annotation expr'
    return (AST.Fix (p, t') expr')
  annotateType t (AST.NatRec p n z s) = do
    n' <- checkType (Type.fromAST' AST.TypeNat) n
    z' <- annotateType t z

    s' <- case snd $ annotation z' of
      (Just (Type t')) -> do
        let f = Type $ AST.TypeFun () [AST.TypeNat ()] (AST.TypeFun () [t'] t')
        checkType f s
      Nothing ->
        inferType s

    let t' = snd $ annotation z'

    return $ AST.NatRec (p, t') n' z' s'
  annotateType _ x@(AST.Fold {}) = do
    tell [notImplemented (annotation x) "Fold"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Unfold {}) = do
    tell [notImplemented (annotation x) "Unfold"]
    return $ fmap (,Nothing) x
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
          tell [notImplemented p "Non-Zero Integer"]
          return Nothing

    return $ AST.ConstInt (p, t') n
  annotateType _ x@(AST.ConstMemory {}) = do
    tell [notImplemented (annotation x) "ConstMemory"]
    return $ fmap (,Nothing) x
  annotateType t (AST.Var p stellaident@(AST.StellaIdent name)) = do
    context <- get

    t' <- case Context.typeOf name context of
      (Just t'') -> do
        Just <$> liftType' p t'' t
      Nothing -> do
        tell [Context.unknownName p name]
        return Nothing

    return $ AST.Var (p, t') stellaident
