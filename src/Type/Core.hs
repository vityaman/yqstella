module Type.Core (Type (Type), fromAST, fromAST', eqT, neqT, fn) where

import Control.Monad (void)
import PrettyPrint (displayAST)
import qualified SyntaxGen.AbsStella as AST

newtype Type = Type (AST.Type' ()) deriving (Eq, Ord)

instance Show Type where
  show (Type x) = displayAST x

fromAST :: AST.Type' a -> Type
fromAST t = Type $ void t

fromAST' :: (() -> AST.Type' ()) -> Type
fromAST' t = fromAST $ t ()

eqT :: Type -> (() -> AST.Type' ()) -> Bool
eqT (Type lhs) rhs = lhs == rhs ()

neqT :: Type -> (() -> AST.Type' ()) -> Bool
neqT lhs rhs = not $ eqT lhs rhs

fn :: [Type] -> Type -> Type
fn args (Type return') = Type $ AST.TypeFun () (fmap toAST args) return'
  where
    toAST (Type x) = x
