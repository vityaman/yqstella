import qualified Golden
import Test.Tasty (defaultMain)

main :: IO ()
main = Golden.main >>= defaultMain
