module PrettyPrint (displayAST) where

import qualified Syntax.PrintStella as Syntax

displayAST :: (Syntax.Print a) => a -> String
displayAST = Syntax.printTree
