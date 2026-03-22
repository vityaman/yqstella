module Lexer
  ( AlexToken,
    StellaToken (StellaToken),
    scan,
    display,
  )
where

import Control.Monad (foldM)
import Control.Monad.Writer
import Data.List (singleton)
import Diagnostic.Code (Code (LEXICS))
import Diagnostic.Core (Diagnostic (Diagnostic), Diagnostics, Severity (Error))
import Position (Position (Position), pointRange)
import Syntax.LexStella (Token (Err, PT), tokenText)
import Syntax.ParStella (myLexer)

type AlexToken = Token

newtype StellaToken = StellaToken AlexToken

instance Show StellaToken where
  show (StellaToken token@(PT position _)) =
    ":" ++ show (Position position) ++ "\t" ++ tokenText token
  show (StellaToken (Err _)) = undefined

scan :: String -> Writer Diagnostics [StellaToken]
scan input =
  foldM visit [] $ myLexer input
  where
    visit' :: AlexToken -> Writer Diagnostics (Maybe StellaToken)
    visit' alex = case alex of
      (Err posn) -> do
        _ <- tell [Diagnostic Error LEXICS (pointRange $ Position posn) "illegal token"]
        return Nothing
      token ->
        return $ Just $ StellaToken token

    visit :: [StellaToken] -> AlexToken -> Writer Diagnostics [StellaToken]
    visit acc alex = do
      token' <- visit' alex
      return $ acc ++ maybe [] singleton token'

display :: [StellaToken] -> String
display = unlines . map show
