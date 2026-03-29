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
    withSourcePreview,
    notImplemented,
  )
where

import Data.Array (Array, (!))
import Data.Char (toTitle)
import Diagnostic.Code (Code (..))
import Diagnostic.Position (Position (Position), PositionRange (PositionRange), pointRange)
import Misc.Display (Display (display))
import SyntaxGen.LexStella (Posn (Pn))

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

withSourcePreview :: Array Int String -> Diagnostic -> Diagnostic
withSourcePreview lines' (Diagnostic s c r@(PositionRange b _) m) =
  Diagnostic s c r (m ++ preview)
  where
    (Position (Pn _ line column)) = b
    line' = (if line == 0 then 1 else line) - 1
    column' = (if column == 0 then 1 else column) - 1
    lineS = lines' ! line'
    prefix = "> "
    indent = replicate (column' + length prefix) ' '
    preview = "\n" ++ prefix ++ lineS ++ "\n" ++ indent ++ "^"

notImplemented :: Position -> String -> Diagnostic
notImplemented position feature =
  let message' = feature ++ " not implemented"
   in Diagnostic Fatal NOT_IMPLEMENTED (pointRange position) message'
