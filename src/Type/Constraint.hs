module Type.Constraint (Constraint (Eq), Constraints) where

import Diagnostic.Position (Position)
import Type.Core (Type)

data Constraint = Eq Position Type Type

type Constraints = [Constraint]

instance Show Constraint where
  show (Eq _ lhs rhs) = show lhs ++ " == " ++ show rhs
