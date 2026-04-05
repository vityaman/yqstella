module Yson.Decode (decodeYson) where

import qualified Yson.SyntaxGen.AbsYson as AST
import qualified Yson.SyntaxGen.ParYson as Par
import qualified Yson.Tree as Yson

decodeYson :: String -> Either String Yson.Tree
decodeYson input = case Par.pTree $ Par.myLexer input of
  Left err -> Left err
  Right tree -> Right $ tree' tree

tree' :: AST.Tree' a -> Yson.Tree
tree' (AST.ATree _ attrs object) =
  let attributes'' = attributes' attrs
      object'' = object' object
   in Yson.Tree attributes'' object''

attributes' :: AST.Attributes' a -> Yson.Attributes
attributes' (AST.SomeAttributes _ fragment) =
  let fragment'' = fragment' fragment
   in Yson.Attributes fragment''
attributes' (AST.NoneAttributes _) =
  Yson.Attributes []

scalar' :: AST.Scalar' a -> Yson.Object
scalar' (AST.ScalarString _ ystring) = Yson.String $ ystring' ystring
scalar' (AST.ScalarInt _ integer) = Yson.Int $ integer' integer
scalar' (AST.ScalarDouble _ double) = Yson.Double $ double' double
scalar' (AST.ScalarBoolean _ boolean) = Yson.Boolean $ boolean' boolean

ystring' :: AST.YString' a -> String
ystring' (AST.StringQuoted _ str) = str
ystring' (AST.StringIdentifier _ (AST.Indentifier ident)) = ident

integer' :: Integer -> Integer
integer' = id

double' :: Double -> Rational
double' = toRational

boolean' :: AST.Boolean' a -> Bool
boolean' (AST.BooleanFalse _) = False
boolean' (AST.BooleanTrue _) = True

list' :: AST.List' a -> Yson.Object
list' (AST.AList _ listItems) =
  let trees = fmap listItem' listItems
   in Yson.List trees

listItem' :: AST.ListItem' a -> Yson.Tree
listItem' (AST.AListItem _ tree) = tree' tree

map' :: AST.Map' a -> Yson.Object
map' (AST.AMap _ mapFragment) =
  let pairs = fragment' mapFragment
   in Yson.Map pairs

object' :: AST.Object' a -> Yson.Object
object' (AST.ObjectScalar _ scalar) = scalar' scalar
object' (AST.ObjectEntity _ _) = Yson.Entity
object' (AST.ObjectList _ list) = list' list
object' (AST.ObjectMap _ mapVal) = map' mapVal

fragment' :: AST.MapFragment' a -> [(String, Yson.Tree)]
fragment' (AST.AMapFragment _ pairs) = map keyValuePair' pairs

keyValuePair' :: AST.KeyValuePair' a -> (String, Yson.Tree)
keyValuePair' (AST.AKeyValuePair _ str tree) = (ystring' str, tree' tree)
