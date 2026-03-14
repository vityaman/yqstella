module Main (main) where

import qualified CLI
import Control.Monad.State
import qualified Diagnostic
import qualified Lexer

main :: IO ()
main = do
  CLI.Args
    { CLI.inputPath = inputPath,
      CLI.diagnosticsPath = diagnosticsPath,
      CLI.tokensPath = tokensPath
    } <-
    CLI.parseArgs

  content <- readFile inputPath

  let (tokens, diagnostics) = runState (Lexer.scan content) []

  writeFile diagnosticsPath (Diagnostic.displays diagnostics)
  writeFile tokensPath (Lexer.display tokens)

  return ()
