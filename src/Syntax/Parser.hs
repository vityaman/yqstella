{-# LANGUAGE FlexibleInstances #-}

module Syntax.Parser (parse) where

import Control.Monad.Writer
import Diagnostic.Code (Code (SYNTAX))
import Diagnostic.Core (Diagnostics, Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange, position)
import qualified Diagnostic.Position as Position
import Syntax.Lexer (StellaToken (StellaToken))
import SyntaxGen.AbsStella (BNFC'Position, Program')
import SyntaxGen.ParStella (pProgram)

parse :: [StellaToken] -> Writer Diagnostics (Maybe (Program' Position))
parse tokens = case pProgram $ fmap (\(StellaToken x) -> x) tokens of
  (Right program) ->
    return $ Just $ fmap toStellaPosition program
  (Left message) -> do
    _ <- tell [diagnostic Error SYNTAX (pointRange Position.unknown) message]
    return Nothing
  where
    toStellaPosition :: BNFC'Position -> Position
    toStellaPosition Nothing = Position.unknown
    toStellaPosition (Just (line, column)) = position line column
