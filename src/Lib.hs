module Lib
  ( Source (Source),
    Project (Project),
    diagnostics,
    tokens,
    parseTree,
    build,
  )
where

import Control.Monad.Writer
import Diagnostic (Diagnostics)
import Extension.Activation (activateExtensions)
import qualified Lexer
import qualified Parser
import Position (Position)
import qualified Syntax.AbsStella as Parser

newtype Source = Source String

data Project = Project
  { diagnostics :: Diagnostics,
    tokens :: [Lexer.StellaToken],
    parseTree :: Maybe (Parser.Program' Position)
  }

build :: Source -> Project
build (Source source) =
  let (project, diagnostics') = runWriter $ do
        tokens' <- Lexer.scan source
        parseTree' <- Parser.parse tokens'
        _ <- maybe (return ()) activateExtensions parseTree'
        return $
          Project
            { diagnostics = [],
              tokens = tokens',
              parseTree = parseTree'
            }
   in project {diagnostics = diagnostics'}
