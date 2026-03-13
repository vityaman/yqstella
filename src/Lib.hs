module Lib
  ( someFunc,
  )
where

import Syntax.AbsStella ()
import Syntax.ParStella (myLexer, pProgram)

someFunc :: IO ()
someFunc = do
  input <- getContents
  let tokens = myLexer input
  let tree = pProgram tokens
  let output = either id show tree
  putStrLn output
