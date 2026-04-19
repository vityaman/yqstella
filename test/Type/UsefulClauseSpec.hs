module Type.UsefulClauseSpec (main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Type.UsefulClause (C, P (C, W), PT ((:::)), i)

ctors :: String -> Map C [String]
ctors "Bool" = Map.fromList [("True", []), ("False", [])]
ctors "Maybe Bool" = Map.fromList [("Some", ["Bool"]), ("None", [])]
ctors t = error $ "unknown type " ++ show t

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

main :: IO TestTree
main =
  return $
    testGroup
      "UsefulClause"
      [ testExample1,
        testExample2,
        testExample3,
        testExample4,
        testExample5
      ]
