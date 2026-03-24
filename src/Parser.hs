{-# LANGUAGE FlexibleInstances #-}

module Parser (parse) where

import Control.Monad.Writer
import Diagnostic.Code (Code (SYNTAX))
import Diagnostic.Core (Diagnostic (Diagnostic), Diagnostics, Severity (Error))
import Diagnostic.Position (Position, pointRange, position)
import qualified Diagnostic.Position as Position
import Lexer (StellaToken (StellaToken))
import Syntax.AbsStella (BNFC'Position, Program')
import Syntax.ParStella (pProgram)

parse :: [StellaToken] -> Writer Diagnostics (Maybe (Program' Position))
parse tokens = case pProgram $ fmap (\(StellaToken x) -> x) tokens of
  (Right program) ->
    return $ Just $ fmap toStellaPosition program
  (Left message) -> do
    _ <- tell [Diagnostic Error SYNTAX (pointRange Position.unknown) message]
    return Nothing
  where
    toStellaPosition :: BNFC'Position -> Position
    toStellaPosition Nothing = Position.unknown
    toStellaPosition (Just (line, column)) = position line column
