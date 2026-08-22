module Type.Constraint (Constraint (Eq), Constraints) where

import Diagnostic.Position (Position)
import Type.Core (Type)

data Constraint = Eq Position Type Type

type Constraints = [Constraint]

instance Show Constraint where
  show (Eq p lhs rhs) = "(" ++ show p ++ ") " ++ show lhs ++ " == " ++ show rhs
