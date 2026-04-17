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
import Syntax.PrettyPrint (displayAST)
import qualified SyntaxGen.AbsStella as AST

newtype Type = Type (AST.Type' ()) deriving (Eq, Ord)

instance Show Type where
  show (Type x) = displayAST x

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
