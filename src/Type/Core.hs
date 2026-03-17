module Type.Core (Type (Type), fromAST, fn) where

import Control.Monad (void)
import PrettyPrint (printTree)
import qualified Syntax.AbsStella as AST

newtype Type = Type (AST.Type' ()) deriving (Eq, Ord)

instance Show Type where
  show (Type x) = printTree x

fromAST :: AST.Type' a -> Type
fromAST t = Type $ void t

fn :: [Type] -> Type -> Type
fn args (Type return') = Type $ AST.TypeFun () (fmap toAST args) return'
  where
    toAST (Type x) = x
