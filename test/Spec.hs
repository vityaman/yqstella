import qualified Golden
import Test.Tasty (defaultMain, testGroup)
import qualified YsonSpec

main :: IO ()
main = do
  goldenTests <- Golden.main
  ysonTests <- YsonSpec.main
  defaultMain $ testGroup "All Tests" [goldenTests, ysonTests]
