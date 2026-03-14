module Diagnostic
  ( Severity (Error),
    Diagnostic (Diagnostic),
    severity,
    range,
    message,
    Diagnostics,
    display,
    displays,
  )
where

import Position (PositionRange (PositionRange))

data Severity = Error deriving (Show)

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
