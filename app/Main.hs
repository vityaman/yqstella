module Main (main) where

import qualified CLI

import Lib

main :: IO ()
main = do
  args <- CLI.parseArgs
  print args
  return ()
