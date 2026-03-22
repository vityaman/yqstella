module Diagnostic.Core
  ( Severity (Fatal, Error),
    Diagnostic (Diagnostic),
    severity,
    code,
    range,
    message,
    Diagnostics,
    display,
    displays,
    notImplemented,
  )
where

import Data.Char (toTitle)
import Diagnostic.Code (Code (..))
import Position (Position, PositionRange (PositionRange), pointRange)

data Severity
  = Fatal
  | Error
  deriving (Show)

data Diagnostic = Diagnostic
  { severity :: Severity,
    code :: Code,
    range :: PositionRange,
    message :: String
  }

type Diagnostics = [Diagnostic]

display :: Diagnostic -> String
display (Diagnostic s c (PositionRange b _) m) =
  "<source>:" ++ show b ++ ": " ++ withSeverity s c ++ ": " ++ m
  where
    withSeverity s' c' = map toTitle (show s') ++ "_" ++ show c'

displays :: Diagnostics -> String
displays = unlines . map display

notImplemented :: Position -> String -> Diagnostic
notImplemented position feature =
  let message' = feature ++ " not implemented"
   in Diagnostic Fatal NOT_IMPLEMENTED (pointRange position) message'
