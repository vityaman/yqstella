{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module CLI (Args, parseArgs) where

import Options.Applicative
  ( Parser,
    execParser,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    showDefault,
    strOption,
    value,
    (<**>),
  )

import System.FilePath (FilePath)

data Args = Args
  { inputPath :: FilePath,
    diagnosticsPath :: FilePath,
    tokensPath :: FilePath,
    parseTreePath :: FilePath,
    typeAnnotationPath :: FilePath,
    outputPath :: FilePath
  } deriving (Show)

parseArgs :: IO Args
parseArgs =
  let description :: String
      description = "Compiles a YQStella source into YQLs"

      header' :: String
      header' = "YQStella - an experimental YQL frontend"

      args :: Parser Args
      args = do
        inputPath <-
          strOption
            ( long "input-path"
                <> metavar "INPUT_PATH"
                <> showDefault
                <> value "/dev/stdin"
                <> help "Where to read YQStella source"
            )

        diagnosticsPath <-
          strOption
            ( long "diagnostics-path"
                <> metavar "DIAGNOSTICS_PATH"
                <> showDefault
                <> value "/dev/stderr"
                <> help "Where to write diagnostics"
            )

        tokensPath <-
          strOption
            ( long "tokens-path"
                <> metavar "TOKENS_PATH"
                <> showDefault
                <> value "/dev/null"
                <> help "Where to write tokens"
            )

        parseTreePath <-
          strOption
            ( long "parse-tree-path"
                <> metavar "PARSE_TREE_PATH"
                <> showDefault
                <> value "/dev/null"
                <> help "Where to write parse tree"
            )

        typeAnnotationPath <-
          strOption
            ( long "typeann-path"
                <> metavar "TYPEANN_PATH"
                <> showDefault
                <> value "/dev/null"
                <> help "Where to write type annotations"
            )

        outputPath <-
          strOption
            ( long "output-path"
                <> metavar "OUTPUT_PATH"
                <> showDefault
                <> value "/dev/stdout"
                <> help "Where to write YQLS output"
            )

        return Args {..}

      opts =
        info
          (args <**> helper)
          (progDesc description <> header header')
   in execParser opts
