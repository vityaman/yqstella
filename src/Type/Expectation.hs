module Type.Expectation
  ( TypeKind (..),
    sanitizeT,
    sanitizeTSilent,
    validateTypeParameters,
    liftEqType,
    liftEqType',
    ensureEqType,
    ensureEqParamType,
    listItemType,
    commonType,
    mismatch,
    mismatchSS,
  )
where

import Control.Monad (when)
import Control.Monad.State (get, gets)
import Data.Functor (void)
import Data.List (groupBy, intercalate)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (mapMaybe)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Diagnostic, Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange, unknown)
import qualified Extension.Core as Extension
import Misc.Duplicate (sepUniqDupBy)
import qualified SyntaxGen.AbsStella as AST
import qualified Type.Constraint as Constraint
import qualified Type.Context as Context
import Type.Core (Type (Type), toAST)
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, freshTypeVar, isAvailable, tellC, tellD, validateUniqueBy, withStateTAE)
import qualified Type.Unification as Unification

data TypeKind = Expected | Inferred

-- | Like 'sanitizeT' but omits duplicate record\/variant field diagnostics. Use
-- in the annotation pass (and 'Type.Decl.toParamSilent') after 'sanitizeT' has
-- already run while building the context, so those errors are not reported twice.
sanitizeTSilent :: AST.Type' Position -> TypeAnnotationEnv Type
sanitizeTSilent = sanitizeT' False

sanitizeT :: AST.Type' Position -> TypeAnnotationEnv Type
sanitizeT = sanitizeT' True

validateTypeParameters :: Position -> [AST.StellaIdent] -> TypeAnnotationEnv [AST.StellaIdent]
validateTypeParameters = validateTypeParameters' True

validateTypeParameters' :: Bool -> Position -> [AST.StellaIdent] -> TypeAnnotationEnv [AST.StellaIdent]
validateTypeParameters' reporting p = validateUniqueBy reporting name toDiagnostic
  where
    name (AST.StellaIdent value) = value
    toDiagnostic parameter =
      let message = "duplicate type parameter: " ++ name parameter
       in diagnostic Error DUPLICATE_TYPE_PARAMETER (pointRange p) message

sanitizeT' :: Bool -> AST.Type' Position -> TypeAnnotationEnv Type
sanitizeT' _ (AST.TypeAuto _) =
  freshTypeVar
sanitizeT' reporting (AST.TypeFun _ args ret) = do
  args' <- fmap toAST <$> mapM (sanitizeT' reporting) args
  ret' <- toAST <$> sanitizeT' reporting ret
  return $ Type $ AST.TypeFun () args' ret'
sanitizeT' reporting (AST.TypeForAll p parameters body) = do
  parameters' <- validateTypeParameters' reporting p parameters
  context <- get
  let (resolved, context') = Context.bindTypeVariables parameters' context
  body' <- withStateTAE (const context') (sanitizeT' reporting body)
  return $ Type $ AST.TypeForAll () resolved (toAST body')
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
sanitizeT' reporting (AST.TypeVar p (AST.StellaIdent name)) = do
  context <- get
  case Context.resolveTypeVariable name context of
    Just resolved -> return $ Type $ AST.TypeVar () resolved
    Nothing -> case Context.typeWithAlias name context of
      Just t -> return t
      Nothing -> do
        let message = "undefined type variable " ++ name
        when reporting $ tellD [diagnostic Error UNDEFINED_TYPE_VARIABLE (pointRange p) message]
        return $ Type $ AST.TypeVar () (AST.StellaIdent name)

-- TODO: make it return Maybe Type
liftEqType :: Position -> (() -> AST.Type' ()) -> Maybe Type -> TypeAnnotationEnv Type
liftEqType p lifting expected = do
  isTypeReconstruction <- isAvailable Extension.TypeReconstruction
  isUniversalTypes <- isAvailable Extension.UniversalTypes
  ensureEqType p (mismatchFor (isTypeReconstruction && not isUniversalTypes) UNEXPECTED_TYPE_FOR_EXPRESSION p) (Type $ lifting ()) expected

liftEqType' :: Position -> Type -> Maybe Type -> TypeAnnotationEnv Type
liftEqType' p lifting expected = do
  isTypeReconstruction <- isAvailable Extension.TypeReconstruction
  isUniversalTypes <- isAvailable Extension.UniversalTypes
  ensureEqType p (mismatchFor (isTypeReconstruction && not isUniversalTypes) UNEXPECTED_TYPE_FOR_EXPRESSION p) lifting expected

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
ensureEqType _ _ lifting (Just checked)
  | Unification.alphaEq lifting checked =
      return lifting
ensureEqType p toDiagnostic lifting (Just checked) = do
  metaVariables <- gets Context.metaVars
  if null (Unification.freeMetaVars metaVariables lifting <> Unification.freeMetaVars metaVariables checked)
    then when (lifting /= checked) $ tellD [toDiagnostic checked lifting]
    else tellC [Constraint.Eq p lifting checked]
  return lifting
ensureEqType _ _ lifting Nothing =
  pure lifting

listItemType :: Position -> TypeKind -> Maybe Type -> TypeAnnotationEnv (Maybe Type)
listItemType _ _ (Just (Type (AST.TypeList () t))) =
  return $ Just $ Type t
listItemType p kind (Just variable@(Type (AST.TypeVar () _))) = do
  metaVariables <- gets Context.metaVars
  if Unification.isMetaVar metaVariables variable
    then do
      (Type item) <- freshTypeVar
      tellC [Constraint.Eq p variable (Type (AST.TypeList () item))]
      return $ Just $ Type item
    else unexpectedListItemType p kind variable
listItemType p kind (Just t) = unexpectedListItemType p kind t
listItemType _ _ Nothing =
  return Nothing

unexpectedListItemType :: Position -> TypeKind -> Type -> TypeAnnotationEnv (Maybe Type)
unexpectedListItemType p Inferred t = do
  let message = "expected list, got " ++ show t
  tellD [diagnostic Error NOT_A_LIST (pointRange p) message]
  return Nothing
unexpectedListItemType p Expected t = do
  isTypeReconstruction <- isAvailable Extension.TypeReconstruction
  let message = "expected " ++ show t ++ ", got list"
      code = if isTypeReconstruction then UNEXPECTED_TYPE_FOR_EXPRESSION else UNEXPECTED_LIST
  tellD [diagnostic Error code (pointRange p) message]
  return Nothing

commonType :: Position -> [(Position, Maybe Type)] -> TypeAnnotationEnv (Maybe Type)
commonType p pts = do
  metaVariables <- gets Context.metaVars
  let present = mapMaybe snd pts
      hasMetaVariables = not $ all (null . Unification.freeMetaVars metaVariables) present

  if hasMetaVariables
    then case present of
      [] -> return Nothing
      first : rest -> do
        mapM_ (ensureEqType p (mismatch UNEXPECTED_TYPE_FOR_EXPRESSION p) first . Just) rest
        return $ Just first
    else do
      let groups =
            map
              (\((p', t') :| rest) -> (t', p' : map fst rest))
              (mapMaybe nonEmpty (groupBy (\(_, a) (_, b) -> areSame a b) pts))

      case groups of
        [(Nothing, _)] ->
          return Nothing
        [(Nothing, _), (Just t', _)] ->
          return $ Just t'
        [(Just t', _)] ->
          return $ Just t'
        ts -> do
          let ts' = fmap (maybe "?" show . fst) ts
              message = "expected same type for all subexpressions, got " ++ intercalate ", " ts'
          tellD [diagnostic Error UNEXPECTED_TYPE_FOR_EXPRESSION (pointRange p) message]
          return Nothing
  where
    areSame Nothing Nothing = True
    areSame (Just lhs) (Just rhs) = Unification.alphaEq lhs rhs
    areSame _ _ = False

mismatch :: Code -> Position -> Type -> Type -> Diagnostic
mismatch code p expected@(Type (AST.TypeList () _)) actual@(Type (AST.TypeList () _)) =
  mismatchSS code p (show expected) (show actual)
mismatch _ p expected@(Type (AST.TypeList () _)) actual@(Type _) =
  mismatchSS NOT_A_LIST p (show expected) (show actual)
mismatch code p expected actual =
  mismatchSS code p (show expected) (show actual)

mismatchFor :: Bool -> Code -> Position -> Type -> Type -> Diagnostic
mismatchFor True _ p expected actual =
  mismatchSS UNEXPECTED_TYPE_FOR_EXPRESSION p (show expected) (show actual)
mismatchFor False code p expected actual =
  mismatch code p expected actual

mismatchSS :: Code -> Position -> String -> String -> Diagnostic
mismatchSS code p expected actual =
  let message = "type mismatch: expected " ++ expected ++ ", got " ++ actual
   in diagnostic Error code (pointRange p) message
