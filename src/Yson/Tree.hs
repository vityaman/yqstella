module Yson.Tree
  ( Tree (Tree),
    Attributes (Attributes),
    Object (String, Int, Double, Boolean, Entity, List, Map),
  )
where

data Tree = Tree Attributes Object
  deriving (Show, Eq)

newtype Attributes = Attributes [(String, Tree)]
  deriving (Show, Eq)

data Object
  = String String
  | Int Integer
  | Double Rational
  | Boolean Bool
  | Entity
  | List [Tree]
  | Map [(String, Tree)]
  deriving (Show, Eq)
