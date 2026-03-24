module YQL.AST (Node (..), Show) where

data Node
  = Y [Node]
  | Q Node
  | A String

instance Show Node where
  showsPrec _ (Y xs) = showString "(" . showList' xs . showString ")"
  showsPrec _ (Q x) = showString "'" . shows x
  showsPrec _ (A x) = showString x

showList' :: (Show a) => [a] -> String -> String
showList' [] = showString ""
showList' [x] = shows x
showList' (x : xs') = shows x . showString " " . showList' xs'
