{-# LANGUAGE FlexibleInstances #-}

module Syntax.Parser (parse) where

import Control.Monad.Writer
import Data.Foldable (asum)
import Data.List (isPrefixOf, stripPrefix, tails)
import Data.Maybe (fromMaybe, listToMaybe)
import Diagnostic.Code (Code (SYNTAX))
import Diagnostic.Core (Diagnostics, Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange, position, unknown)
import qualified Diagnostic.Position as Position
import Syntax.Lexer (StellaToken (StellaToken))
import SyntaxGen.AbsStella (BNFC'Position, Program')
import SyntaxGen.ParStella (pProgram)

parseBNFCMessage :: String -> Position
parseBNFCMessage msg =
  fromMaybe unknown (asum (map try (tails msg)))
  where
    try :: String -> Maybe Position
    try s
      | "line " `isPrefixOf` s = do
          (l, r1) <- listToMaybe (reads (drop 5 s))
          r2 <- stripPrefix ", column " r1
          (c, _) <- listToMaybe (reads r2)
          return (position l c)
      | otherwise = Nothing

parse :: [StellaToken] -> Writer Diagnostics (Maybe (Program' Position))
parse tokens = case pProgram $ fmap (\(StellaToken x) -> x) tokens of
  (Right program) ->
    return $ Just $ fmap toStellaPosition program
  (Left message) -> do
    let p = parseBNFCMessage message
    _ <- tell [diagnostic Error SYNTAX (pointRange p) message]
    return Nothing
  where
    toStellaPosition :: BNFC'Position -> Position
    toStellaPosition Nothing = Position.unknown
    toStellaPosition (Just (line, column)) = position line column
