module Type.Unification (unify, alphaEq, freeMetaVars, isMetaVar, checkAmbiguity) where

import Control.Monad (zipWithM)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Diagnostic, Diagnostics, Severity (Error, Fatal), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Constraint
import Type.Core (Type (Type), fv)
import Type.Substitution (Substitution)
import qualified Type.Substitution as Substitution

unify :: Set String -> Constraints -> Either Diagnostic Substitution
unify _ [] =
  return Substitution.empty
unify metaVars (Eq _ s t : cs) | alphaEq s t = unify metaVars cs
unify metaVars (Eq p (Type (AST.TypeSum () lhsS rhsS)) (Type (AST.TypeSum () lhsT rhsT)) : cs) = do
  let cs' = [Eq p (Type lhsS) (Type lhsT), Eq p (Type rhsS) (Type rhsT)]
  unify metaVars $ cs' ++ cs
unify metaVars (Eq p (Type (AST.TypeTuple () ss)) (Type (AST.TypeTuple () ts)) : cs)
  | length ss == length ts = do
      let cs' = [Eq p (Type s) (Type t) | (s, t) <- zip ss ts]
      unify metaVars $ cs' ++ cs
  | otherwise =
      unificationError UNEXPECTED_TUPLE_LENGTH p $
        "can't unify tuples of different lengths: " ++ show (length ss) ++ " and " ++ show (length ts)
unify metaVars (Eq p (Type (AST.TypeRecord () ss)) (Type (AST.TypeRecord () ts)) : cs)
  | Map.keysSet smap == Map.keysSet tmap = do
      let cs' = [Eq p s t | (s, t) <- zip (Map.elems smap) (Map.elems tmap)]
      unify metaVars $ cs' ++ cs
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
unify metaVars (Eq p (Type (AST.TypeVariant () ss)) (Type (AST.TypeVariant () ts)) : cs)
  | Map.keysSet smap == Map.keysSet tmap,
    Just cs' <- zipWithMVariantFields p (Map.elems smap) (Map.elems tmap) =
      unify metaVars $ cs' ++ cs
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
unify metaVars (Eq p (Type (AST.TypeList () s)) (Type (AST.TypeList () t)) : cs) =
  unify metaVars $ Eq p (Type s) (Type t) : cs
unify metaVars (Eq p (Type (AST.TypeRef () s)) (Type (AST.TypeRef () t)) : cs) =
  unify metaVars $ Eq p (Type s) (Type t) : cs
unify _ (Eq p lhs@(Type (AST.TypeForAll () _ _)) rhs@(Type (AST.TypeForAll () _ _)) : _) =
  unificationError UNEXPECTED_TYPE_FOR_EXPRESSION p $ "can't unify " ++ show lhs ++ " and " ++ show rhs
unify metaVars (Eq p (Type (AST.TypeVar () (AST.StellaIdent x))) t : cs)
  | isMetaVar metaVars (Type (AST.TypeVar () (AST.StellaIdent x))),
    not $ x `Set.member` fv t = do
      subst <- unify metaVars (fmap (Substitution.applyConstraint $ Substitution.singleton x t) cs)
      return $ Substitution.insert x t subst
  | isMetaVar metaVars (Type (AST.TypeVar () (AST.StellaIdent x))) = do
      let message = "mu " ++ x ++ ". " ++ show t
      Left $ diagnostic Error OCCURS_CHECK_INFINITE_TYPE (pointRange p) message
unify metaVars (Eq p s (Type (AST.TypeVar () (AST.StellaIdent x))) : cs)
  | isMetaVar metaVars (Type (AST.TypeVar () (AST.StellaIdent x))),
    not $ x `Set.member` fv s = do
      subst <- unify metaVars (fmap (Substitution.applyConstraint $ Substitution.singleton x s) cs)
      return $ Substitution.insert x s subst
  | isMetaVar metaVars (Type (AST.TypeVar () (AST.StellaIdent x))) = do
      let message = "mu " ++ x ++ ". " ++ show s
      Left $ diagnostic Error OCCURS_CHECK_INFINITE_TYPE (pointRange p) message
unify metaVars (Eq p (Type (AST.TypeFun () ss s)) (Type (AST.TypeFun () ts t)) : cs)
  | length ss == length ts = do
      let ss' = fmap Type $ s : ss
          ts' = fmap Type $ t : ts
          cs' = uncurry (Eq p) <$> zip ss' ts'
      unify metaVars $ cs' ++ cs
  | otherwise =
      unificationError INCORRECT_NUMBER_OF_ARGUMENTS p $
        "can't unify functions with different arities: " ++ show (length ss) ++ " and " ++ show (length ts)
unify _ (Eq p s t : _) =
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

alphaEq :: Type -> Type -> Bool
alphaEq (Type lhs) (Type rhs) = equal Map.empty Map.empty 0 lhs rhs
  where
    equal :: Map String Int -> Map String Int -> Int -> AST.Type' () -> AST.Type' () -> Bool
    equal leftBindings rightBindings depth left right =
      case (left, right) of
        (AST.TypeAuto (), AST.TypeAuto ()) -> True
        (AST.TypeFun () leftArgs leftResult, AST.TypeFun () rightArgs rightResult) ->
          length leftArgs == length rightArgs
            && and (zipWith (equal leftBindings rightBindings depth) leftArgs rightArgs)
            && equal leftBindings rightBindings depth leftResult rightResult
        (AST.TypeForAll () leftParameters leftBody, AST.TypeForAll () rightParameters rightBody)
          | length leftParameters == length rightParameters ->
              let levels = [depth .. depth + length leftParameters - 1]
                  leftBindings' = foldr (uncurry Map.insert) leftBindings (zip (fmap name leftParameters) levels)
                  rightBindings' = foldr (uncurry Map.insert) rightBindings (zip (fmap name rightParameters) levels)
               in equal leftBindings' rightBindings' (depth + length leftParameters) leftBody rightBody
        (AST.TypeRec () leftParameter leftBody, AST.TypeRec () rightParameter rightBody) ->
          equal
            (Map.insert (name leftParameter) depth leftBindings)
            (Map.insert (name rightParameter) depth rightBindings)
            (depth + 1)
            leftBody
            rightBody
        (AST.TypeSum () leftLhs leftRhs, AST.TypeSum () rightLhs rightRhs) ->
          equal leftBindings rightBindings depth leftLhs rightLhs
            && equal leftBindings rightBindings depth leftRhs rightRhs
        (AST.TypeTuple () leftTypes, AST.TypeTuple () rightTypes) ->
          length leftTypes == length rightTypes
            && and (zipWith (equal leftBindings rightBindings depth) leftTypes rightTypes)
        (AST.TypeRecord () leftFields, AST.TypeRecord () rightFields) ->
          equalRecordFields leftBindings rightBindings depth leftFields rightFields
        (AST.TypeVariant () leftFields, AST.TypeVariant () rightFields) ->
          equalVariantFields leftBindings rightBindings depth leftFields rightFields
        (AST.TypeList () leftItem, AST.TypeList () rightItem) ->
          equal leftBindings rightBindings depth leftItem rightItem
        (AST.TypeBool (), AST.TypeBool ()) -> True
        (AST.TypeNat (), AST.TypeNat ()) -> True
        (AST.TypeUnit (), AST.TypeUnit ()) -> True
        (AST.TypeTop (), AST.TypeTop ()) -> True
        (AST.TypeBottom (), AST.TypeBottom ()) -> True
        (AST.TypeRef () leftItem, AST.TypeRef () rightItem) ->
          equal leftBindings rightBindings depth leftItem rightItem
        (AST.TypeVar () leftVariable, AST.TypeVar () rightVariable) ->
          case (Map.lookup (name leftVariable) leftBindings, Map.lookup (name rightVariable) rightBindings) of
            (Just leftLevel, Just rightLevel) -> leftLevel == rightLevel
            (Nothing, Nothing) -> leftVariable == rightVariable
            _ -> False
        _ -> False

    equalRecordFields leftBindings rightBindings depth leftFields rightFields =
      length leftFields == length rightFields
        && and
          [ leftName == rightName && equal leftBindings rightBindings depth leftType rightType
            | (AST.ARecordFieldType () leftName leftType, AST.ARecordFieldType () rightName rightType) <- zip leftFields rightFields
          ]

    equalVariantFields leftBindings rightBindings depth leftFields rightFields =
      length leftFields == length rightFields
        && and
          [ leftName == rightName && equalOptional leftType rightType
            | (AST.AVariantFieldType () leftName leftType, AST.AVariantFieldType () rightName rightType) <- zip leftFields rightFields
          ]
      where
        equalOptional (AST.NoTyping ()) (AST.NoTyping ()) = True
        equalOptional (AST.SomeTyping () leftType) (AST.SomeTyping () rightType) =
          equal leftBindings rightBindings depth leftType rightType
        equalOptional _ _ = False

    name (AST.StellaIdent value) = value

freeMetaVars :: Set String -> Type -> Set String
freeMetaVars metaVars = Set.intersection metaVars . fv

isMetaVar :: Set String -> Type -> Bool
isMetaVar metaVars (Type (AST.TypeVar () (AST.StellaIdent name))) = name `Set.member` metaVars
isMetaVar _ _ = False

checkAmbiguity :: Set String -> AST.Program' (Position, Maybe Type) -> Diagnostics
checkAmbiguity metaVars = foldMap checkAnnotation
  where
    checkAnnotation :: (Position, Maybe Type) -> Diagnostics
    checkAnnotation (_, Nothing) = mempty
    checkAnnotation (position, Just (Type (AST.TypeAuto ()))) =
      [diagnostic Fatal AMBIGUOUS_TYPE (pointRange position) "unexpected auto type after substitution"]
    checkAnnotation (position, Just type_) =
      fmap (ambiguousTypeVariable position type_) (Set.toList $ freeMetaVars metaVars type_)

    ambiguousTypeVariable position type_ variable =
      let message = "type " ++ show type_ ++ " has unresolved type variable " ++ variable
       in diagnostic Error AMBIGUOUS_TYPE (pointRange position) message
