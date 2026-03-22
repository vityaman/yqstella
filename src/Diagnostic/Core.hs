module Diagnostic.Core
  ( Severity (Fatal, Error),
    Diagnostic (Diagnostic),
    severity,
    range,
    message,
    Diagnostics,
    display,
    displays,
    notImplemented,
  )
where

import Position (Position, PositionRange (PositionRange), pointRange)

data Severity
  = Fatal
  | Error
  deriving (Show)

data Diagnostic = Diagnostic
  { severity :: Severity,
    range :: PositionRange,
    message :: String
  }

type Diagnostics = [Diagnostic]

display :: Diagnostic -> String
display (Diagnostic s (PositionRange b _) m) =
  show s ++ " at :" ++ show b ++ ": " ++ m

displays :: Diagnostics -> String
displays = unlines . map display

notImplemented :: Position -> String -> Diagnostic
notImplemented position feature =
  let message' = feature ++ " not implemented"
   in Diagnostic Fatal (pointRange position) message'
