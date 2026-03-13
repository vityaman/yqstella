module Lib
    ( someFunc
    ) where

import Syntax.ParStella ( pProgram, myLexer )

someFunc :: IO ()
someFunc = putStrLn "someFunc"
