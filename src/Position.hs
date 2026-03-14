module Position
  ( Position (Position),
    PositionRange (PositionRange),
    pointRange,
  )
where

import Syntax.LexStella (Posn (Pn))

newtype Position = Position Posn

data PositionRange = PositionRange Position Position

instance Show Position where
  show (Position (Pn _ l c)) = show l ++ ":" ++ show c

pointRange :: Position -> PositionRange
pointRange x = PositionRange x x
