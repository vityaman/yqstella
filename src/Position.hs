module Position
  ( Position (Position),
    PositionRange (PositionRange),
    position,
    unknown,
    pointRange,
  )
where

import Syntax.LexStella (Posn (Pn))

newtype Position = Position Posn

data PositionRange = PositionRange Position Position

instance Show Position where
  show (Position (Pn _ l c)) = show l ++ ":" ++ show c

position :: Int -> Int -> Position
position line column = Position (Pn 0 line column)

unknown :: Position
unknown = position 1 0

pointRange :: Position -> PositionRange
pointRange x = PositionRange x x
