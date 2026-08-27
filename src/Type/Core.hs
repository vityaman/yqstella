module Type.Core
  ( Type (Type),
    fromAST,
    fromAST',
    toAST,
    eqT,
    neqT,
    fn,
    list,
    fv,
  )
where

import Control.Monad (void)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified SyntaxGen.AbsStella as AST

newtype Type = Type (AST.Type' ()) deriving (Eq, Ord)

instance Show Type where
  show (Type x) = go 0 x
    where
      prec :: AST.Type' () -> Int
      prec AST.TypeFun {} = 0
      prec AST.TypeForAll {} = 0
      prec AST.TypeRec {} = 0
      prec AST.TypeSum {} = 1
      prec AST.TypeTuple {} = 2
      prec AST.TypeRecord {} = 2
      prec AST.TypeVariant {} = 2
      prec AST.TypeList {} = 2
      prec AST.TypeRef {} = 2
      prec _ = 3

      parensIf :: Bool -> String -> String
      parensIf True s = "(" ++ s ++ ")"
      parensIf False s = s

      go :: Int -> AST.Type' () -> String
      go ctx t = parensIf (prec t < ctx) rendered
        where
          rendered = case t of
            AST.TypeAuto _ ->
              "auto"
            AST.TypeFun _ args ret ->
              "fn(" ++ intercalate ", " (map (go 0) args) ++ ") -> " ++ go 0 ret
            AST.TypeForAll _ idents body ->
              "forall " ++ intercalate ", " (map prettyN idents) ++ ". " ++ go 0 body
            AST.TypeRec _ ident body ->
              "µ " ++ prettyN ident ++ ". " ++ go 0 body
            AST.TypeSum _ lhs rhs ->
              go 2 lhs ++ " + " ++ go 2 rhs
            AST.TypeTuple _ ts ->
              "{" ++ intercalate ", " (map (go 0) ts) ++ "}"
            AST.TypeRecord _ fields ->
              "{" ++ intercalate ", " (map prettyRF fields) ++ "}"
            AST.TypeVariant _ fields ->
              "<| " ++ intercalate ", " (map prettyVF fields) ++ " |>"
            AST.TypeList _ item ->
              "[" ++ go 0 item ++ "]"
            AST.TypeRef _ t2 ->
              "&" ++ go 2 t2
            AST.TypeBool _ ->
              "Bool"
            AST.TypeNat _ ->
              "Nat"
            AST.TypeUnit _ ->
              "Unit"
            AST.TypeTop _ ->
              "Top"
            AST.TypeBottom _ ->
              "Bot"
            AST.TypeVar _ ident ->
              prettyN ident

      prettyN :: AST.StellaIdent -> String
      prettyN (AST.StellaIdent s) = s

      prettyRF :: AST.RecordFieldType' () -> String
      prettyRF (AST.ARecordFieldType _ n t) =
        prettyN n ++ " : " ++ go 0 t

      prettyVF :: AST.VariantFieldType' () -> String
      prettyVF (AST.AVariantFieldType _ n (AST.NoTyping _)) =
        prettyN n
      prettyVF (AST.AVariantFieldType _ n (AST.SomeTyping _ t')) =
        prettyN n ++ " : " ++ go 0 t'

fromAST :: AST.Type' a -> Type
fromAST t = Type $ void t

fromAST' :: (() -> AST.Type' ()) -> Type
fromAST' t = fromAST $ t ()

toAST :: Type -> AST.Type' ()
toAST (Type x) = x

eqT :: Type -> (() -> AST.Type' ()) -> Bool
eqT (Type lhs) rhs = lhs == rhs ()

neqT :: Type -> (() -> AST.Type' ()) -> Bool
neqT lhs rhs = not $ eqT lhs rhs

fn :: [Type] -> Type -> Type
fn args (Type returntype) = Type $ AST.TypeFun () (fmap toAST args) returntype

list :: Type -> Type
list (Type t) = Type $ AST.TypeList () t

fv :: Type -> Set String
fv (Type t') = go Set.empty t'
  where
    go bound t =
      case t of
        AST.TypeAuto () -> mempty
        AST.TypeFun () args ret -> Set.unions $ go bound ret : fmap (go bound) args
        AST.TypeForAll () xs body -> go (bound <> names xs) body
        AST.TypeRec () x body -> go (Set.insert (name x) bound) body
        AST.TypeSum () lhs rhs -> go bound lhs <> go bound rhs
        AST.TypeTuple () types -> Set.unions $ fmap (go bound) types
        AST.TypeRecord () fields -> Set.unions $ fmap (rf bound) fields
        AST.TypeVariant () fields -> Set.unions $ fmap (vf bound) fields
        AST.TypeList () item -> go bound item
        AST.TypeBool () -> mempty
        AST.TypeNat () -> mempty
        AST.TypeUnit () -> mempty
        AST.TypeTop () -> mempty
        AST.TypeBottom () -> mempty
        AST.TypeRef () item -> go bound item
        AST.TypeVar () ident
          | name ident `Set.member` bound -> mempty
          | otherwise -> Set.singleton $ name ident

    names = Set.fromList . fmap name
    name (AST.StellaIdent value) = value

    rf bound (AST.ARecordFieldType () _ t) = go bound t

    vf _ (AST.AVariantFieldType () _ (AST.NoTyping ())) = mempty
    vf bound (AST.AVariantFieldType () _ (AST.SomeTyping () t)) = go bound t
