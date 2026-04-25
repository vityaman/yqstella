module Type.Core
  ( Type (Type),
    fromAST,
    fromAST',
    toAST,
    eqT,
    neqT,
    fn,
    list,
    fromReturn,
  )
where

import Control.Monad (void)
import Data.List (intercalate)
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
              "forall " ++ unwords (map prettyN idents) ++ ". " ++ go 0 body
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

fromReturn :: AST.ReturnType' a -> Maybe Type
fromReturn (AST.NoReturnType _) = Nothing
fromReturn (AST.SomeReturnType _ t) = Just $ fromAST t
