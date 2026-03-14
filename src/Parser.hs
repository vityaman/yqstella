{-# LANGUAGE FlexibleInstances #-}

module Parser (StellaParseTree (StellaParseTree), parse) where

import Control.Monad.Writer
import Diagnostic (Diagnostic (Diagnostic), Diagnostics, Severity (Error))
import Lexer (StellaToken (StellaToken))
import Position (Position, pointRange, position)
import qualified Position
import Syntax.AbsStella (BNFC'Position, Program')
import Syntax.ParStella (pProgram)

newtype StellaParseTree f a = StellaParseTree (f a)

parse :: [StellaToken] -> Writer Diagnostics (Maybe (StellaParseTree Program' Position))
parse tokens = case pProgram $ fmap (\(StellaToken x) -> x) tokens of
  (Right program) ->
    return $ Just $ StellaParseTree $ fmap toStellaPosition program
  (Left message) -> do
    _ <- tell [Diagnostic Error (pointRange Position.unknown) message]
    return Nothing
  where
    toStellaPosition :: BNFC'Position -> Position
    toStellaPosition Nothing = Position.unknown
    toStellaPosition (Just (line, column)) = position line column
