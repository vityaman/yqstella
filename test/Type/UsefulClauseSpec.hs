module Type.UsefulClauseSpec (main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Type.UsefulClause (C, P (C, W), PT ((:::)), i)

ctors :: String -> Map C [String]
ctors "Bool" =
  Map.fromList [("True", []), ("False", [])]
ctors "Maybe Bool" =
  Map.fromList [("Some", ["Bool"]), ("None", [])]
ctors "Tuple<Nat, Bool>" =
  Map.fromList [("tuple", ["Nat", "Bool"])]
ctors "Variant<a: Unit, b: Bool>" =
  Map.fromList [("a", ["Unit"]), ("b", ["Bool"])]
ctors "Unit" =
  Map.fromList [("unit", [])]
ctors "Nat + (Bool + (Nat -> Nat))" =
  Map.fromList [("inl", ["Nat"]), ("inr", ["Bool + (Nat -> Nat)"])]
ctors "Bool + (Nat -> Nat)" =
  Map.fromList [("inl", ["Bool"]), ("inr", ["Nat -> Nat"])]
ctors "Nat" =
  Map.fromList [("0", []), ("succ", ["Nat"])]
ctors "Nat -> Nat" =
  Map.empty
ctors t =
  error $ "unknown type " ++ show t

testExample1 :: TestTree
testExample1 =
  testCase "Example1 - Exhaustive match on Bool" $
    assertEqual "" expected actual
  where
    matrix =
      [ [C "True" [] ::: "Bool"],
        [C "False" [] ::: "Bool"]
      ]
    actual = i ctors ["Bool"] matrix
    expected = Nothing

testExample2 :: TestTree
testExample2 =
  testCase "Example2 - Non-exhaustive match on Bool" $
    assertEqual "" expected actual
  where
    matrix =
      [ [C "True" [] ::: "Bool"]
      ]
    actual = i ctors ["Bool"] matrix
    expected = Just [C "False" [] ::: "Bool"]

testExample3 :: TestTree
testExample3 =
  testCase "Example3 - Deep Pattern Matching on Option Bool" $
    assertEqual "" expected actual
  where
    matrix =
      [ [C "Some" [C "True" [] ::: "Bool"] ::: "Maybe Bool"],
        [C "None" [] ::: "Maybe Bool"]
      ]
    actual = i ctors ["Maybe Bool"] matrix
    expected = Just [C "Some" [C "False" [] ::: "Bool"] ::: "Maybe Bool"]

testExample4 :: TestTree
testExample4 =
  testCase "Example4 - Missing variant with wildcard" $
    assertEqual "" expected actual
  where
    matrix =
      [ [C "Some" [W ::: "Bool"] ::: "Maybe Bool"]
      ]
    actual = i ctors ["Maybe Bool"] matrix
    expected = Just [C "None" [] ::: "Maybe Bool"]

testExample5 :: TestTree
testExample5 =
  testCase "Example5 - Multiple Columns (Tuple matching)" $
    assertEqual "" expected actual
  where
    matrix =
      [ [C "True" [] ::: "Bool", C "True" [] ::: "Bool"],
        [C "False" [] ::: "Bool", W ::: "Bool"],
        [W ::: "Bool", C "False" [] ::: "Bool"]
      ]
    actual = i ctors ["Bool", "Bool"] matrix
    expected = Nothing

testExample6 :: TestTree
testExample6 =
  testCase "Example6 - Exhaustive <| a, b : Bool |> (nullary a + b)" $
    assertEqual "" expected actual
  where
    matrix =
      [ [C "a" [C "unit" [] ::: "Unit"] ::: "Variant<a: Unit, b: Bool>"],
        [C "b" [W ::: "Bool"] ::: "Variant<a: Unit, b: Bool>"]
      ]
    actual = i ctors ["Variant<a: Unit, b: Bool>"] matrix
    expected = Nothing

testExample7 :: TestTree
testExample7 =
  testCase "Example7 - Nested sums: Nat + (Bool + (Nat -> Nat)) exhaustive" $
    assertEqual "" expected actual
  where
    matrix =
      [ [C "inl" [W ::: "Nat"] ::: "Nat + (Bool + (Nat -> Nat))"],
        [C "inr" [C "inl" [W ::: "Bool"] ::: "Bool + (Nat -> Nat)"] ::: "Nat + (Bool + (Nat -> Nat))"],
        [C "inr" [C "inr" [W ::: "Nat -> Nat"] ::: "Bool + (Nat -> Nat)"] ::: "Nat + (Bool + (Nat -> Nat))"]
      ]
    actual = i ctors ["Nat + (Bool + (Nat -> Nat))"] matrix
    expected = Nothing

testExample8 :: TestTree
testExample8 =
  testCase "Example8 - Tuple matching" $
    assertEqual "" expected actual
  where
    matrix =
      [ [C "tuple" [C "0" [] ::: "Nat", W ::: "Bool"] ::: "Tuple<Nat, Bool>"]
      ]
    actual = i ctors ["Tuple<Nat, Bool>"] matrix
    expected = Just [C "tuple" [C "succ" [W ::: "Nat"] ::: "Nat", W ::: "Bool"] ::: "Tuple<Nat, Bool>"]

main :: IO TestTree
main =
  return $
    testGroup
      "UsefulClause"
      [ testExample1,
        testExample2,
        testExample3,
        testExample4,
        testExample5,
        testExample6,
        testExample7,
        testExample8
      ]
