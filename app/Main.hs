module Main (main) where

import qualified CLI
import Control.Monad.Writer (runWriter)
import qualified Diagnostic
import qualified Lexer
import qualified Parser
import Parser (StellaParseTree (StellaParseTree))
import qualified PrettyPrint

main :: IO ()
main = do
  CLI.Args
    { CLI.inputPath = inputPath,
      CLI.diagnosticsPath = diagnosticsPath,
      CLI.tokensPath = tokensPath,
      CLI.parseTreePath = parseTreePath
    } <-
    CLI.parseArgs

  content <- readFile inputPath

  let ((tokens, parseTree), diagnostics) = runWriter $ do
        tokens' <- Lexer.scan content
        parseTree' <- Parser.parse tokens'
        return (tokens', parseTree')

  let formatted' = maybe "" (\(StellaParseTree x) -> PrettyPrint.printTree x) parseTree

  writeFile diagnosticsPath (Diagnostic.displays diagnostics)
  writeFile tokensPath (Lexer.display tokens)
  writeFile parseTreePath formatted'

  return ()
