module Lexer
  ( AlexToken,
    StellaToken (StellaToken),
    scan,
    display,
  )
where

import Control.Monad (foldM)
import Control.Monad.State
import Diagnostic (Diagnostic (Diagnostic), Diagnostics, Severity (Error))
import Position (Position (Position), pointRange)
import Syntax.LexStella (Token (Err, PT), tokenText)
import Syntax.ParStella (myLexer)

type AlexToken = Token

newtype StellaToken = StellaToken AlexToken

instance Show StellaToken where
  show (StellaToken token@(PT position _)) =
    ":" ++ show (Position position) ++ "\t" ++ tokenText token
  show (StellaToken (Err _)) = undefined

scan :: String -> State Diagnostics [StellaToken]
scan input =
  foldM
    ( \ts t -> case t of
        (Err posn) -> do
          modify (Diagnostic Error (pointRange $ Position posn) "illegal token" :)
          return ts
        token -> return (StellaToken token : ts)
    )
    []
    $ myLexer input

display :: [StellaToken] -> String
display = unlines . map show
