module PrettyPrint (displayAST) where

import qualified SyntaxGen.PrintStella as SyntaxGen

displayAST :: (SyntaxGen.Print a) => a -> String
displayAST = SyntaxGen.printTree
