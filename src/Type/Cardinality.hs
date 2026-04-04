module Type.Cardinality (Cardinality, cardinality, (+?+)) where

import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (Type))

type Cardinality = Maybe Integer

cardinality :: Type -> Cardinality
cardinality (Type t) = cardinality' t
  where
    cardinality' t' = case t' of
      AST.TypeSum () inl inr ->
        cardinality (Type inl) +?+ cardinality (Type inr)
      AST.TypeTuple () [] ->
        Just 1
      AST.TypeTuple () (x : xs) ->
        cardinality (Type x) *?* cardinality (Type $ AST.TypeTuple () xs)
      AST.TypeVariant () [] ->
        Just 1
      AST.TypeVariant () (x : xs) ->
        cardinality'' x +?+ cardinality (Type $ AST.TypeVariant () xs)
      AST.TypeBool () ->
        Just 2
      AST.TypeUnit () ->
        Just 1
      _ -> Nothing

    cardinality'' (AST.AVariantFieldType _ _ (AST.NoTyping _)) =
      Just 1
    cardinality'' (AST.AVariantFieldType _ _ (AST.SomeTyping _ t')) =
      cardinality (Type t')

(+?+) :: Cardinality -> Cardinality -> Cardinality
(+?+) Nothing _ = Nothing
(+?+) _ Nothing = Nothing
(+?+) (Just lhs) (Just rhs) = Just $ lhs + rhs

(*?*) :: Cardinality -> Cardinality -> Cardinality
(*?*) Nothing _ = Nothing
(*?*) _ Nothing = Nothing
(*?*) (Just lhs) (Just rhs) = Just $ lhs * rhs
