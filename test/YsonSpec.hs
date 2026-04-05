module YsonSpec (main) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Yson.Decode (decodeYson)
import Yson.Tree (Attributes (Attributes), Object (Map, String), Tree (Tree))

testParameters :: Assertion
testParameters =
  let input = "{__n__ = { Data = \"1\" }}"
      expected =
        Tree
          (Attributes [])
          ( Map
              [ ( "__n__",
                  Tree
                    (Attributes [])
                    (Map [("Data", Tree (Attributes []) (String "1"))])
                )
              ]
          )
   in case decodeYson input of
        Left err -> assertEqual "Should parse successfully" "" err
        Right actual -> assertEqual "Parsed structure should match expected" expected actual

main :: IO TestTree
main =
  return $
    testGroup
      "Yson"
      [ testCase "Parameters" testParameters
      ]
