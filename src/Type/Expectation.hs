module Type.Expectation
  ( TypeKind (..),
    sanitizeT,
    sanitizeTSilent,
    liftType,
    liftType',
    listItemType,
    commonType,
    mismatch,
    mismatchSS,
  )
where

import Control.Monad (when)
import Control.Monad.State (get)
import Control.Monad.Writer (tell)
import Data.Functor (void)
import Data.List (groupBy, intercalate)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (mapMaybe)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Diagnostic, Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange, unknown)
import Misc.Duplicate (sepUniqDupBy)
import qualified SyntaxGen.AbsStella as AST
import qualified Type.Context as Context
import Type.Core (Type (Type))
import Type.Env (TypeAnnotationEnv)

data TypeKind = Expected | Inferred

-- | Like 'sanitizeT' but omits duplicate record\/variant field diagnostics. Use
-- in the annotation pass (and 'Type.Decl.toParamSilent') after 'sanitizeT' has
-- already run while building the context, so those errors are not reported twice.
sanitizeTSilent :: AST.Type' Position -> TypeAnnotationEnv Type
sanitizeTSilent = sanitizeT' False

sanitizeT :: AST.Type' Position -> TypeAnnotationEnv Type
sanitizeT = sanitizeT' True

sanitizeT' :: Bool -> AST.Type' Position -> TypeAnnotationEnv Type
sanitizeT' reporting (AST.TypeRecord _ fields) = do
  let sanitizeF (AST.ARecordFieldType p' n t) = do
        (Type t') <- sanitizeT' reporting t
        return (AST.ARecordFieldType p' n (fmap (const unknown) t'))

  fields' <- mapM sanitizeF fields
  let (uniq, dup) = sepUniqDupBy (\(AST.ARecordFieldType _ n _) -> n) fields'
      toDiagnostic (AST.ARecordFieldType p' (AST.StellaIdent name') _) =
        let message = "duplicate field: " ++ name'
         in diagnostic Error DUPLICATE_RECORD_TYPE_FIELDS (pointRange p') message

  when reporting $ tell $ fmap toDiagnostic dup
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

  when rep $ tell $ fmap toDiagnostic dup
  return $ Type $ AST.TypeVariant () (fmap void uniq)
sanitizeT' _ (AST.TypeVar _ (AST.StellaIdent name)) = do
  context <- get
  case Context.typeWithAlias name context of
    Just t -> return t
    Nothing -> return $ Type $ AST.TypeVar () (AST.StellaIdent name)
sanitizeT' _ t = pure $ Type $ void t

-- TODO: make it return Maybe Type
liftType :: Position -> (() -> AST.Type' ()) -> Maybe Type -> TypeAnnotationEnv Type
liftType p lifting = liftType' p (Type $ lifting ())

liftType' :: Position -> Type -> Maybe Type -> TypeAnnotationEnv Type
liftType' p lifting (Just checked) = do
  when (lifting /= checked) $
    tell [mismatch UNEXPECTED_TYPE_FOR_EXPRESSION p checked lifting]
  return lifting
liftType' _ lifting Nothing =
  pure lifting

listItemType :: Position -> TypeKind -> Maybe Type -> TypeAnnotationEnv (Maybe Type)
listItemType _ _ (Just (Type (AST.TypeList () t))) =
  return $ Just $ Type t
listItemType p Inferred (Just t) = do
  let message = "expected list, got " ++ show t
  tell [diagnostic Error NOT_A_LIST (pointRange p) message]
  return Nothing
listItemType p Expected (Just t) = do
  let message = "expected " ++ show t ++ ", got list"
  tell [diagnostic Error UNEXPECTED_LIST (pointRange p) message]
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
      tell [diagnostic Error UNEXPECTED_TYPE_FOR_EXPRESSION (pointRange p) message]
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
