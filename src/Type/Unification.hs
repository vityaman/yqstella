module Type.Unification (unify) where

import Control.Monad (zipWithM)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Diagnostic, Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Constraint
import Type.Core (Type (Type), fv)
import Type.Substitution (Substitution)
import qualified Type.Substitution as Substitution

unify :: Constraints -> Either Diagnostic Substitution
unify [] =
  return Substitution.empty
unify (Eq _ s t : cs) | s == t = unify cs
unify (Eq p (Type (AST.TypeSum () lhsS rhsS)) (Type (AST.TypeSum () lhsT rhsT)) : cs) = do
  let cs' = [Eq p (Type lhsS) (Type lhsT), Eq p (Type rhsS) (Type rhsT)]
  unify $ cs' ++ cs
unify (Eq p (Type (AST.TypeTuple () ss)) (Type (AST.TypeTuple () ts)) : cs)
  | length ss == length ts = do
      let cs' = [Eq p (Type s) (Type t) | (s, t) <- zip ss ts]
      unify $ cs' ++ cs
  | otherwise =
      unificationError UNEXPECTED_TUPLE_LENGTH p $
        "can't unify tuples of different lengths: " ++ show (length ss) ++ " and " ++ show (length ts)
unify (Eq p (Type (AST.TypeRecord () ss)) (Type (AST.TypeRecord () ts)) : cs)
  | Map.keysSet smap == Map.keysSet tmap = do
      let cs' = [Eq p s t | (s, t) <- zip (Map.elems smap) (Map.elems tmap)]
      unify $ cs' ++ cs
  | otherwise =
      unificationError code p $
        "can't unify records with different fields; unexpected "
          ++ showNames unexpected
          ++ ", missing "
          ++ showNames missing
  where
    smap = Map.fromList $ fmap toPair ss
    tmap = Map.fromList $ fmap toPair ts
    unexpected = Map.keysSet smap `Set.difference` Map.keysSet tmap
    missing = Map.keysSet tmap `Set.difference` Map.keysSet smap
    code = if Set.null unexpected then MISSING_RECORD_FIELDS else UNEXPECTED_RECORD_FIELDS
    toPair (AST.ARecordFieldType () name t) = (name, Type t)
unify (Eq p (Type (AST.TypeVariant () ss)) (Type (AST.TypeVariant () ts)) : cs)
  | Map.keysSet smap == Map.keysSet tmap,
    Just cs' <- zipWithMVariantFields p (Map.elems smap) (Map.elems tmap) =
      unify $ cs' ++ cs
  | Map.keysSet smap /= Map.keysSet tmap =
      unificationError code p $
        "can't unify variants with different labels; unexpected "
          ++ showNames unexpected
          ++ ", missing "
          ++ showNames missing
  | otherwise =
      unificationError UNEXPECTED_TYPE_FOR_EXPRESSION p "can't unify variant labels with different payloads"
  where
    smap = Map.fromList $ fmap toPair ss
    tmap = Map.fromList $ fmap toPair ts
    unexpected = Map.keysSet smap `Set.difference` Map.keysSet tmap
    missing = Map.keysSet tmap `Set.difference` Map.keysSet smap
    code = if Set.null missing then UNEXPECTED_VARIANT_LABEL else MISSING_VARIANT_LABELS
    toPair (AST.AVariantFieldType () name typing) = (name, typing)
unify (Eq p (Type (AST.TypeList () s)) (Type (AST.TypeList () t)) : cs) =
  unify $ Eq p (Type s) (Type t) : cs
unify (Eq p (Type (AST.TypeRef () s)) (Type (AST.TypeRef () t)) : cs) =
  unify $ Eq p (Type s) (Type t) : cs
unify (Eq p (Type (AST.TypeVar () (AST.StellaIdent x))) t : cs)
  | not $ x `Set.member` fv t = do
      subst <- unify (fmap (Substitution.applyConstraint $ Substitution.singleton x t) cs)
      return $ Substitution.insert x t subst
  | otherwise = do
      let message = "mu " ++ x ++ ". " ++ show t
      Left $ diagnostic Error OCCURS_CHECK_INFINITE_TYPE (pointRange p) message
unify (Eq p s (Type (AST.TypeVar () (AST.StellaIdent x))) : cs)
  | not $ x `Set.member` fv s = do
      subst <- unify (fmap (Substitution.applyConstraint $ Substitution.singleton x s) cs)
      return $ Substitution.insert x s subst
  | otherwise = do
      let message = "mu " ++ x ++ ". " ++ show s
      Left $ diagnostic Error OCCURS_CHECK_INFINITE_TYPE (pointRange p) message
unify (Eq p (Type (AST.TypeFun () ss s)) (Type (AST.TypeFun () ts t)) : cs)
  | length ss == length ts = do
      let ss' = fmap Type $ s : ss
          ts' = fmap Type $ t : ts
          cs' = uncurry (Eq p) <$> zip ss' ts'
      unify $ cs' ++ cs
  | otherwise =
      unificationError INCORRECT_NUMBER_OF_ARGUMENTS p $
        "can't unify functions with different arities: " ++ show (length ss) ++ " and " ++ show (length ts)
unify (Eq p s t : _) =
  unificationError UNEXPECTED_TYPE_FOR_EXPRESSION p $ "can't unify " ++ show s ++ " and " ++ show t

unificationError :: Code -> Position -> String -> Either Diagnostic a
unificationError code p message =
  Left $ diagnostic Error code (pointRange p) message

showNames :: Set.Set AST.StellaIdent -> String
showNames = ("{" ++) . (++ "}") . intercalate ", " . fmap name . Set.toAscList
  where
    name (AST.StellaIdent name') = name'

zipWithMVariantFields :: Position -> [AST.OptionalTyping' ()] -> [AST.OptionalTyping' ()] -> Maybe Constraints
zipWithMVariantFields p ss ts = concat <$> zipWithM variantField ss ts
  where
    variantField (AST.NoTyping ()) (AST.NoTyping ()) =
      Just []
    variantField (AST.SomeTyping () s) (AST.SomeTyping () t) =
      Just [Eq p (Type s) (Type t)]
    variantField _ _ =
      Nothing
