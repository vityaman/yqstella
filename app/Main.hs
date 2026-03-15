module Main (main) where

import qualified CLI
import qualified Diagnostic
import qualified Lexer
import qualified PrettyPrint
import qualified Lib as Stella

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

  let project = Stella.build (Stella.Source content)

  let formatted' = maybe "" PrettyPrint.printTree $ Stella.parseTree project

  writeFile diagnosticsPath (Diagnostic.displays $ Stella.diagnostics project)
  writeFile tokensPath (Lexer.display $ Stella.tokens project)
  writeFile parseTreePath formatted'

  return ()
