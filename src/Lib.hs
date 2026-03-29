module Lib
  ( Source (Source),
    Project (Project),
    diagnostics,
    tokens,
    program,
    areTypesCorrect,
    yql,
    build,
  )
where

import Control.Monad.Writer
import qualified Data.Array as Array
import Diagnostic.Core (Diagnostics, withSourcePreview)
import Diagnostic.Position (Position)
import Extension.Activation (activateExtensions)
import qualified Syntax.Lexer as Lexer
import qualified Syntax.Parser as Parser
import qualified SyntaxGen.AbsStella as AST
import Type.Check (checkTypes)
import Type.Core (Type)
import YQL.AST (Node)
import YQL.Translation (toYQL)

newtype Source = Source String

data Project = Project
  { diagnostics :: Diagnostics,
    tokens :: [Lexer.StellaToken],
    program :: Maybe (AST.Program' (Position, Maybe Type)),
    areTypesCorrect :: Bool,
    yql :: Maybe Node
  }

build :: Source -> Project
build (Source source) =
  let (project, diagnostics') = runWriter $ do
        tokens' <- Lexer.scan source
        parseTree' <- Parser.parse tokens'
        _ <- maybe (pure ()) activateExtensions parseTree'

        (areTypesCorrect', program') <- case parseTree' of
          Just parseTree'' -> do
            (areTypesCorrect', program') <- checkTypes parseTree''
            return (areTypesCorrect', Just program')
          Nothing ->
            return (False, Nothing)

        yql' <-
          if not areTypesCorrect'
            then return Nothing
            else case fmap toYQL program' of
              Just (Right x) ->
                return $ Just x
              Just (Left x) -> do
                tell [x]
                return Nothing
              Nothing ->
                return Nothing

        return $
          Project
            { diagnostics = [],
              tokens = tokens',
              program = program',
              areTypesCorrect = areTypesCorrect',
              yql = yql'
            }

      linesLst = lines source
      linesArr = Array.listArray (0, length linesLst - 1) linesLst
      diagnostics'' = fmap (withSourcePreview linesArr) diagnostics'
   in project {diagnostics = diagnostics''}
