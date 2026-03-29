{-# LANGUAGE TupleSections #-}

module Type.Annotation (annotateType, inferType) where

import Annotation (annotation)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard, unless, when, zipWithM)
import Control.Monad.State
import Control.Monad.Writer
import Data.Either (partitionEithers)
import Data.Foldable (find)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Diagnostic, Diagnostics, Severity (Error), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Context (Context)
import qualified Type.Context as Context
import Type.Core (Type (Type))
import qualified Type.Core as AST
import qualified Type.Core as Type

type TypeAnnotationEnv a = WriterT Diagnostics (State Context) a

withStateTAE :: (Context -> Context) -> TypeAnnotationEnv a -> TypeAnnotationEnv a
withStateTAE f m = do
  old <- get
  modify f
  result <- m
  put old
  return result

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

    context' <- get >>= ctxExtendByDecls decls
    decls' <- withStateTAE (const context') (mapM inferType decls)

    let main = find isMain decls'
        type_ = main >>= declFunType'

    when (null main) $ tell [diagnostic Error MISSING_MAIN (pointRange p) "not found: main function"]

    return (AST.AProgram (p, type_) languagedecl' extensions' decls')
    where
      isMain (AST.DeclFun _ _ (AST.StellaIdent name) _ _ _ _ _) | name == "main" = True
      isMain _ = False

      declFunType' (AST.DeclFun (_, type_') _ _ _ _ _ _ _) = type_'
      declFunType' _ = error "Main declaration must be a function"

instance TypeAnnotatable AST.Decl' where
  annotateType _ (AST.DeclFun p annotations stellaident paramdecls returntype throwtype decls expr) = do
    let annotations' = fmap (fmap (,Nothing)) annotations
        paramdecls' = fmap (fmap (,Nothing)) paramdecls
        returntype' = fmap (,Nothing) returntype
        throwtype' = fmap (,Nothing) throwtype
        decls' = fmap (fmap (,Nothing)) decls

    unless (null annotations) $ tell [notImplemented p "DeclFun annotations"]

    unless (null decls) $ tell [notImplemented p "NestedFunctionDeclarations"]

    _ <- case throwtype of
      (AST.NoThrowType _) -> pure ()
      (AST.SomeThrowType _ _) -> tell [notImplemented p "DeclFun ThrowType"]

    let returntype'' = case returntype' of
          (AST.SomeReturnType _ t) -> Just $ Type.fromAST t
          (AST.NoReturnType _) -> Nothing

    context' <- get >>= ctxExtendByParamDecls paramdecls
    expr' <- withStateTAE (const context') (annotateType returntype'' expr)

    let actualType = snd $ annotation expr'
        argTypes = fmap (snd . toPair) paramdecls'
        type' = fmap (Type.fn argTypes) actualType

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
  annotateType _ x@(AST.Let {}) = do
    tell [notImplemented (annotation x) "Let"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.LetRec {}) = do
    tell [notImplemented (annotation x) "LetRec"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.TypeAbstraction {}) = do
    tell [notImplemented (annotation x) "TypeAbstraction"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.LessThan {}) = do
    tell [notImplemented (annotation x) "LessThan"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.LessThanOrEqual {}) = do
    tell [notImplemented (annotation x) "LessThanOrEqual"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.GreaterThan {}) = do
    tell [notImplemented (annotation x) "GreaterThan"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.GreaterThanOrEqual {}) = do
    tell [notImplemented (annotation x) "AST"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Equal {}) = do
    tell [notImplemented (annotation x) "Equal"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.NotEqual {}) = do
    tell [notImplemented (annotation x) "NotEqual"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.TypeAsc {}) = do
    tell [notImplemented (annotation x) "TypeAsc"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.TypeCast {}) = do
    tell [notImplemented (annotation x) "TypeCast"]
    return $ fmap (,Nothing) x
  annotateType t (AST.Abstraction p paramdecls expr) = do
    let paramdecls' = fmap (fmap (,Nothing)) paramdecls
    context' <- get >>= ctxExtendByParamDecls paramdecls

    let infer' = do
          expr' <- withStateTAE (const context') (inferType expr)
          let argTypes = fmap (snd . toPair) paramdecls'
              returnType = snd $ annotation expr'
          return (fmap (Type.fn argTypes) returnType, expr')

    (t', expr') <- case t of
      Just t'@(Type (AST.TypeFun () argTypes returnType)) -> do
        let actual = fmap (\x -> (annotation x, toPair x)) paramdecls
            expected = fmap Type.fromAST argTypes

            toDiagnostic ((p', (name, actual')), expected') = do
              guard $ actual' /= expected'
              let m = "(" ++ name ++ " : " ++ show actual' ++ ")"
              return $ mismatchSS UNEXPECTED_TYPE_FOR_PARAMETER p' (show expected') m

        tell $ mapMaybe toDiagnostic (zip actual expected)
        expr' <- withStateTAE (const context') (checkType (Type returnType) expr)

        return (Just t', expr')
      Just t'' -> do
        (t', expr') <- infer'
        tell [mismatchSS UNEXPECTED_LAMBDA p (show t'') (maybe "lambda" show t')]
        return (t', expr')
      Nothing ->
        infer'

    return $ AST.Abstraction (p, t') paramdecls' expr'
  annotateType _ x@(AST.Variant {}) = do
    tell [notImplemented (annotation x) "Variant"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Match {}) = do
    tell [notImplemented (annotation x) "Match"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.List {}) = do
    tell [notImplemented (annotation x) "List"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Add {}) = do
    tell [notImplemented (annotation x) "Add"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Subtract {}) = do
    tell [notImplemented (annotation x) "Subtract"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.LogicOr {}) = do
    tell [notImplemented (annotation x) "LogicOr"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Multiply {}) = do
    tell [notImplemented (annotation x) "Multiply"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Divide {}) = do
    tell [notImplemented (annotation x) "Divide"]
    return $ fmap (,Nothing) x
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
        let argTypes' = fmap Type argTypes
            returnType' = Type returnType

        xs' <- zipWithM annotateType (fmap Just argTypes') xs
        return (xs', Just returnType')
      Just actual -> do
        xs' <- mapM inferType xs

        let unknown = Type.fromAST' AST.TypeAuto
            expectedArgTypes = fmap (fromMaybe unknown . snd . annotation) xs'
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
  annotateType _ x@(AST.DotRecord {}) = do
    tell [notImplemented (annotation x) "DotRecord"]
    return $ fmap (,Nothing) x
  annotateType t (AST.DotTuple p expr index) = do
    expr' <- inferType expr

    t' <- case snd $ annotation expr' of
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
        pure Nothing

    return (AST.DotTuple (p, t') expr' index)
  annotateType t (AST.Tuple p exprs) = do
    let untype (Type x) = x

    exprs' <- case t of
      Just (Type (AST.TypeTuple _ ts)) -> do
        zipWithM checkType (fmap Type ts) exprs
      Just expected -> do
        exprs' <- mapM inferType exprs

        let unknown = AST.TypeAuto ()
            actualTypes = fmap (maybe unknown untype . snd . annotation) exprs'
            actual = Type.fromAST $ AST.TypeTuple () actualTypes

        let message = "type mismatch: expected " ++ show expected ++ ", got " ++ show actual
        tell [diagnostic Error UNEXPECTED_TUPLE (pointRange p) message]

        return exprs'
      Nothing ->
        mapM inferType exprs

    let t' = Type . AST.TypeTuple () <$> traverse (fmap untype . snd . annotation) exprs'

    return (AST.Tuple (p, t') exprs')
  annotateType _ x@(AST.Record {}) = do
    tell [notImplemented (annotation x) "Record"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.ConsList {}) = do
    tell [notImplemented (annotation x) "ConsList"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Head {}) = do
    tell [notImplemented (annotation x) "Head"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.IsEmpty {}) = do
    tell [notImplemented (annotation x) "IsEmpty"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Tail {}) = do
    tell [notImplemented (annotation x) "Tail"]
    return $ fmap (,Nothing) x
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
  annotateType _ x@(AST.Inl {}) = do
    tell [notImplemented (annotation x) "Inl"]
    return $ fmap (,Nothing) x
  annotateType _ x@(AST.Inr {}) = do
    tell [notImplemented (annotation x) "Inr"]
    return $ fmap (,Nothing) x
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
  annotateType _ x@(AST.Fix {}) = do
    tell [notImplemented (annotation x) "Fix"]
    return $ fmap (,Nothing) x
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
      if n == 0
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

toPair :: AST.ParamDecl' a -> (String, Type)
toPair (AST.AParamDecl _ (AST.StellaIdent key) t) = (key, Type.fromAST t)

ctxExtendByParamDecls :: [AST.ParamDecl' Position] -> Context -> TypeAnnotationEnv Context
ctxExtendByParamDecls paramdecls context = do
  let paramdecls' = Map.fromListWith (++) $ fmap toPair' paramdecls
      toPair' x@(AST.AParamDecl _ (AST.StellaIdent name) _) = (name, [x])

      duplicates =
        [ decl
          | (_, decls) <- Map.toList paramdecls',
            1 < length decls,
            decl <- decls
        ]

      toDiagnostic (AST.AParamDecl p (AST.StellaIdent name) _) =
        let message = "duplicate parameter: " ++ name
         in diagnostic Error DUPLICATE_FUNCTION_PARAMETER (pointRange p) message

      withDecl paramdecl = let (key, t) = toPair paramdecl in Context.withTyped key t

  tell $ fmap toDiagnostic duplicates
  return $ foldr withDecl context paramdecls

ctxExtendByDecls :: [AST.Decl' Position] -> Context -> TypeAnnotationEnv Context
ctxExtendByDecls decls context = do
  let (diagnostics, kpvs) = partitionEithers $ fmap visit decls

      duplicates =
        [ (name, position)
          | (name, pvs) <- Map.toList $ Map.fromListWith (++) kpvs,
            1 < length pvs,
            (position, _) <- pvs
        ]

      toDiagnostic (name, p) =
        let message = "duplicate declaration: " ++ name
         in diagnostic Error DUPLICATE_FUNCTION_DECLARATION (pointRange p) message

      kvs = fmap unpack kpvs
      unpack (k, [(_, t)]) = (k, t)
      unpack _ = undefined

  tell $ diagnostics ++ fmap toDiagnostic duplicates
  return $ foldr (uncurry Context.withTyped) context kvs
  where
    visit :: AST.Decl' Position -> Either Diagnostic (String, [(Position, Type)])
    visit (AST.DeclFun p _ (AST.StellaIdent name) paramdecls (AST.SomeReturnType _ returntype) _ _ _) =
      Right (name, [(p, Type.fn args' returntype')])
      where
        args' = fmap (snd . toPair) paramdecls
        returntype' = Type.fromAST returntype
    visit (AST.DeclFun p _ (AST.StellaIdent name) _ (AST.NoReturnType _) _ _ _) =
      Left $ notImplemented p $ "name resolution for DeclFun " ++ name ++ " due to implicit return type"
    visit (AST.DeclFunGeneric p _ (AST.StellaIdent name) _ _ _ _ _ _) =
      Left $ notImplemented p $ "name resolution for DeclFunGeneric " ++ name
    visit (AST.DeclTypeAlias p (AST.StellaIdent name) _) =
      Left $ notImplemented p $ "name resolution for DeclTypeAlias " ++ name
    visit (AST.DeclExceptionType p type_) =
      Left $ notImplemented p $ "name resolution for DeclExceptionType " ++ show (AST.fromAST type_)
    visit (AST.DeclExceptionVariant p (AST.StellaIdent name) _) =
      Left $ notImplemented p $ "name resolution for DeclExceptionVariant " ++ show name

liftType :: Position -> (() -> AST.Type' ()) -> Maybe Type -> TypeAnnotationEnv Type
liftType p lifting = liftType' p (Type $ lifting ())

liftType' :: Position -> Type -> Maybe Type -> TypeAnnotationEnv Type
liftType' p lifting (Just checked) = do
  when (lifting /= checked) $
    tell [mismatchSS UNEXPECTED_TYPE_FOR_EXPRESSION p (show checked) (show lifting)]
  return lifting
liftType' _ lifting Nothing =
  pure lifting

mismatch :: Code -> Position -> Type -> Type -> Diagnostic
mismatch code p expected actual = mismatchSS code p (show expected) (show actual)

mismatchSS :: Code -> Position -> String -> String -> Diagnostic
mismatchSS code p expected actual =
  let message = "type mismatch: expected " ++ expected ++ ", got " ++ actual
   in diagnostic Error code (pointRange p) message
