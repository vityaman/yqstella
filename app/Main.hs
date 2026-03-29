module Main (main) where

import qualified CLI
import Diagnostic.Core (display)
import Lib (Project (Project))
import qualified Lib as Stella
import Syntax.PrettyPrint (displayAST)
import System.Exit (exitFailure, exitSuccess)
import YQL.PrettyPrint (displayYQL)

main :: IO ()
main = do
  CLI.Args
    { CLI.inputPath = inputPath,
      CLI.diagnosticsPath = diagnosticsPath,
      CLI.tokensPath = tokensPath,
      CLI.parseTreePath = parseTreePath,
      CLI.outputPath = outputPath
    } <-
    CLI.parseArgs

  content <- readFile inputPath

  let Project
        { Stella.diagnostics = diagnostics,
          Stella.tokens = tokens,
          Stella.program = program,
          Stella.areTypesCorrect = areTypesCorrect,
          Stella.yql = yql
        } = Stella.build (Stella.Source content)

      diagnostics' = display diagnostics
      tokens' = display tokens
      formatted' = displayAST <$> program
      yql' = displayYQL <$> yql

      isOk = areTypesCorrect && not (null yql)

  writeFile diagnosticsPath diagnostics'
  writeFile tokensPath tokens'
  writeFile' parseTreePath formatted'
  writeFile' outputPath yql'

  if isOk
    then exitSuccess
    else exitFailure

writeFile' :: FilePath -> Maybe String -> IO ()
writeFile' path (Just content) = writeFile path content
writeFile' _ Nothing = pure ()
