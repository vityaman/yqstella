import qualified Golden
import Test.Tasty (defaultMain, testGroup)
import qualified Type.UsefulClauseSpec
import qualified YsonSpec

main :: IO ()
main = do
  goldenTests <- Golden.main
  ysonTests <- YsonSpec.main
  usefulClauseTests <- Type.UsefulClauseSpec.main
  defaultMain $ testGroup "All Tests" [goldenTests, ysonTests, usefulClauseTests]
