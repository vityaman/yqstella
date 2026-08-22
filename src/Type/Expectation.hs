module Type.Expectation
  ( TypeKind (..),
    sanitizeT,
    sanitizeTSilent,
    liftEqType,
    liftEqType',
    ensureEqParamType,
    listItemType,
    commonType,
    mismatch,
    mismatchSS,
  )
where

import Control.Monad (when)
import Control.Monad.State (get)
import Data.Functor (void)
import Data.List (groupBy, intercalate)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (mapMaybe)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Diagnostic, Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange, unknown)
import Misc.Duplicate (sepUniqDupBy)
import qualified SyntaxGen.AbsStella as AST
import qualified Type.Constraint as Constraint
import qualified Type.Context as Context
import Type.Core (Type (Type), toAST)
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, freshTypeVar, tellC, tellD)

data TypeKind = Expected | Inferred

-- | Like 'sanitizeT' but omits duplicate record\/variant field diagnostics. Use
-- in the annotation pass (and 'Type.Decl.toParamSilent') after 'sanitizeT' has
-- already run while building the context, so those errors are not reported twice.
sanitizeTSilent :: AST.Type' Position -> TypeAnnotationEnv Type
sanitizeTSilent = sanitizeT' False

sanitizeT :: AST.Type' Position -> TypeAnnotationEnv Type
sanitizeT = sanitizeT' True

sanitizeT' :: Bool -> AST.Type' Position -> TypeAnnotationEnv Type
sanitizeT' _ (AST.TypeAuto _) =
  freshTypeVar
sanitizeT' reporting (AST.TypeFun _ args ret) = do
  args' <- fmap toAST <$> mapM (sanitizeT' reporting) args
  ret' <- toAST <$> sanitizeT' reporting ret
  return $ Type $ AST.TypeFun () args' ret'
sanitizeT' reporting t@(AST.TypeForAll p _ _) = do
  when reporting $ tellD [diagnostic Fatal NOT_IMPLEMENTED (pointRange p) "ForAll"]
  return (Type.fromAST t)
sanitizeT' reporting t@(AST.TypeRec p _ _) = do
  when reporting $ tellD [diagnostic Fatal NOT_IMPLEMENTED (pointRange p) "TypeRec"]
  return (Type.fromAST t)
sanitizeT' reporting (AST.TypeSum _ lhs rhs) = do
  lhs' <- toAST <$> sanitizeT' reporting lhs
  rhs' <- toAST <$> sanitizeT' reporting rhs
  return $ Type $ AST.TypeSum () lhs' rhs'
sanitizeT' reporting (AST.TypeTuple _ ts) = do
  types' <- fmap toAST <$> mapM (sanitizeT' reporting) ts
  return $ Type $ AST.TypeTuple () types'
sanitizeT' reporting (AST.TypeRecord _ fields) = do
  let sanitizeF (AST.ARecordFieldType p' n t) = do
        (Type t') <- sanitizeT' reporting t
        return (AST.ARecordFieldType p' n (fmap (const unknown) t'))

  fields' <- mapM sanitizeF fields
  let (uniq, dup) = sepUniqDupBy (\(AST.ARecordFieldType _ n _) -> n) fields'
      toDiagnostic (AST.ARecordFieldType p' (AST.StellaIdent name') _) =
        let message = "duplicate field: " ++ name'
         in diagnostic Error DUPLICATE_RECORD_TYPE_FIELDS (pointRange p') message

  when reporting $ tellD $ fmap toDiagnostic dup
  return $ Type $ AST.TypeRecord () (fmap void uniq)
sanitizeT' rep (AST.TypeVariant _ fields) = do
  let sanitizeF (AST.AVariantFieldType p' n (AST.SomeTyping p'' t)) = do
        (Type t') <- sanitizeT' rep t
        return (AST.AVariantFieldType p' n (AST.SomeTyping p'' (fmap (const unknown) t')))
      sanitizeF t = pure t

  fields' <- mapM sanitizeF fields
  let (uniq, dup) = sepUniqDupBy (\(AST.AVariantFieldType _ n _) -> n) fields'
      toDiagnostic (AST.AVariantFieldType p' (AST.StellaIdent name') _) =
        let message = "duplicate field: " ++ name'
         in diagnostic Error DUPLICATE_VARIANT_TYPE_FIELDS (pointRange p') message

  when rep $ tellD $ fmap toDiagnostic dup
  return $ Type $ AST.TypeVariant () (fmap void uniq)
sanitizeT' reporting (AST.TypeList _ t) = do
  item' <- toAST <$> sanitizeT' reporting t
  return $ Type $ AST.TypeList () item'
sanitizeT' _ (AST.TypeBool _) =
  pure $ Type $ AST.TypeBool ()
sanitizeT' _ (AST.TypeNat _) =
  pure $ Type $ AST.TypeNat ()
sanitizeT' _ (AST.TypeUnit _) =
  pure $ Type $ AST.TypeUnit ()
sanitizeT' _ (AST.TypeTop _) =
  pure $ Type $ AST.TypeTop ()
sanitizeT' _ (AST.TypeBottom _) =
  pure $ Type $ AST.TypeBottom ()
sanitizeT' reporting (AST.TypeRef _ t) = do
  referenced' <- toAST <$> sanitizeT' reporting t
  return $ Type $ AST.TypeRef () referenced'
sanitizeT' _ (AST.TypeVar _ (AST.StellaIdent name)) = do
  context <- get
  case Context.typeWithAlias name context of
    Just t -> return t
    Nothing -> return $ Type $ AST.TypeVar () (AST.StellaIdent name)

-- TODO: make it return Maybe Type
liftEqType :: Position -> (() -> AST.Type' ()) -> Maybe Type -> TypeAnnotationEnv Type
liftEqType p lifting = liftEqType' p (Type $ lifting ())

liftEqType' :: Position -> Type -> Maybe Type -> TypeAnnotationEnv Type
liftEqType' p = ensureEqType p (mismatch UNEXPECTED_TYPE_FOR_EXPRESSION p)

ensureEqParamType :: Position -> String -> Type -> Type -> TypeAnnotationEnv ()
ensureEqParamType p name actual expected =
  void $ ensureEqType p toDiagnostic actual (Just expected)
  where
    toDiagnostic expected' actual' =
      let parameter = "(" ++ name ++ " : " ++ show actual' ++ ")"
       in mismatchSS UNEXPECTED_TYPE_FOR_PARAMETER p (show expected') parameter

ensureEqType :: Position -> (Type -> Type -> Diagnostic) -> Type -> Maybe Type -> TypeAnnotationEnv Type
ensureEqType p _ lifting (Just (Type (AST.TypeAuto ()))) = do
  let message = "unexpected checked auto type, must be a type var"
  tellD [diagnostic Fatal NOT_IMPLEMENTED (pointRange p) message]
  return lifting
ensureEqType p _ lifting@(Type (AST.TypeAuto ())) _ = do
  let message = "unexpected lifting auto type, must be a type var"
  tellD [diagnostic Fatal NOT_IMPLEMENTED (pointRange p) message]
  return lifting
ensureEqType p _ variable@(Type (AST.TypeVar () _)) (Just checked) = do
  tellC [Constraint.Eq p variable checked]
  return variable
ensureEqType p _ lifting (Just variable@(Type (AST.TypeVar () _))) = do
  tellC [Constraint.Eq p lifting variable]
  return lifting
ensureEqType _ toDiagnostic lifting (Just checked) = do
  when (lifting /= checked) $
    tellD [toDiagnostic checked lifting]
  return lifting
ensureEqType _ _ lifting Nothing =
  pure lifting

listItemType :: Position -> TypeKind -> Maybe Type -> TypeAnnotationEnv (Maybe Type)
listItemType _ _ (Just (Type (AST.TypeList () t))) =
  return $ Just $ Type t
listItemType p Inferred (Just t) = do
  let message = "expected list, got " ++ show t
  tellD [diagnostic Error NOT_A_LIST (pointRange p) message]
  return Nothing
listItemType p Expected (Just t) = do
  let message = "expected " ++ show t ++ ", got list"
  tellD [diagnostic Error UNEXPECTED_LIST (pointRange p) message]
  return Nothing
listItemType _ _ Nothing =
  return Nothing

commonType :: Position -> [(Position, Maybe Type)] -> TypeAnnotationEnv (Maybe Type)
commonType p pts = do
  let groups =
        map
          (\((p', t') :| rest) -> (t', p' : map fst rest))
          (mapMaybe nonEmpty (groupBy (\(_, a) (_, b) -> a == b) pts))

  case groups of
    [(Nothing, _)] ->
      return Nothing
    [(Nothing, _), (Just t', _)] ->
      return $ Just t'
    [(Just t', _)] ->
      return $ Just t'
    ts -> do
      -- TODO(vityaman): improve diagnostic
      let ts' = fmap (maybe "?" show . fst) ts
          message = "expected same type for all subexpressions, got " ++ intercalate ", " ts'
      tellD [diagnostic Error UNEXPECTED_TYPE_FOR_EXPRESSION (pointRange p) message]
      return Nothing

mismatch :: Code -> Position -> Type -> Type -> Diagnostic
mismatch code p expected@(Type (AST.TypeList () _)) actual@(Type (AST.TypeList () _)) =
  mismatchSS code p (show expected) (show actual)
mismatch _ p expected@(Type (AST.TypeList () _)) actual@(Type _) =
  mismatchSS NOT_A_LIST p (show expected) (show actual)
mismatch code p expected actual =
  mismatchSS code p (show expected) (show actual)

mismatchSS :: Code -> Position -> String -> String -> Diagnostic
mismatchSS code p expected actual =
  let message = "type mismatch: expected " ++ expected ++ ", got " ++ actual
   in diagnostic Error code (pointRange p) message
