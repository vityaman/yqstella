import qualified Golden
import Test.Tasty (defaultMain, testGroup)
import qualified YsonSpec
import qualified Type.UsefulClauseSpec

main :: IO ()
main = do
  goldenTests <- Golden.main
  ysonTests <- YsonSpec.main
  usefulClauseTests <- Type.UsefulClauseSpec.main
  defaultMain $ testGroup "All Tests" [goldenTests, ysonTests, usefulClauseTests]
