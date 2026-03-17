module PrettyPrint (printTree) where

import qualified Syntax.PrintStella as Syntax

printTree :: (Syntax.Print a) => a -> String
printTree = Syntax.printTree
