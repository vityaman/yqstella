{-# LANGUAGE FlexibleInstances #-}

module Diagnostic.Core
  ( Severity (Fatal, Error),
    Diagnostic (Diagnostic),
    severity,
    code,
    range,
    message,
    Diagnostics,
    display,
    notImplemented,
  )
where

import Data.Char (toTitle)
import Diagnostic.Code (Code (..))
import Diagnostic.Position (Position, PositionRange (PositionRange), pointRange)
import Misc.Display (Display (display))

data Severity
  = Fatal
  | Error
  deriving (Show, Eq)

data Diagnostic = Diagnostic
  { severity :: Severity,
    code :: Code,
    range :: PositionRange,
    message :: String
  }

type Diagnostics = [Diagnostic]

instance Display Diagnostic where
  display (Diagnostic s c (PositionRange b _) m) =
    "<source>:" ++ show b ++ ": " ++ withSeverity s c ++ ": " ++ m
    where
      withSeverity s' c' = map toTitle (show s') ++ "_" ++ show c'

instance Display Diagnostics where
  display = unlines . map display

notImplemented :: Position -> String -> Diagnostic
notImplemented position feature =
  let message' = feature ++ " not implemented"
   in Diagnostic Fatal NOT_IMPLEMENTED (pointRange position) message'
