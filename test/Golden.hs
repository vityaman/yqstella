module Golden (main) where

import Control.Monad (filterM)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (toUpper)
import Data.List (sort)
import qualified Diagnostic
import qualified Lib as Stella
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, takeFileName, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

newtype Input = Input String

newtype Output = Output
  { diagnostics :: String
  }

test :: Input -> Output
test (Input source) =
  let project = Stella.build (Stella.Source source)
   in Output
        { diagnostics = Diagnostic.displays $ Stella.diagnostics project
        }

main :: IO TestTree
main = do
  let goldenPath = "test" </> "golden"
  groupPaths <- ls goldenPath
  groupTests <- traverse makeTestGroup (sort groupPaths)
  return $ testGroup "Golden" groupTests

makeTestGroup :: FilePath -> IO TestTree
makeTestGroup groupPath = do
  let groupName = "Section " ++ takeFileName groupPath
  casePaths <- ls groupPath
  cases <- traverse makeTestCase (sort casePaths)
  return $ testGroup groupName cases

makeTestCase :: FilePath -> IO TestTree
makeTestCase casePath = do
  let caseName = "Test " ++ takeFileName casePath

  input <- Input <$> readFile (casePath </> "input.yqst")

  let Output
        { diagnostics = diagnostics'
        } = test input

  let artifact fileName content =
        let name = toTitleWord $ dropExtension $ takeFileName fileName
         in goldenVsStringDiff name diff (casePath </> fileName) (pure $ pack content)

  let artifacts =
        [ artifact "diagnostics.txt" diagnostics'
        ]

  return $ testGroup caseName artifacts

ls :: FilePath -> IO [FilePath]
ls parent = do
  entries <- listDirectory parent
  filterM doesDirectoryExist ((parent </>) <$> entries)

diff :: String -> String -> [String]
diff ref new = ["diff", ref, new]

toTitleWord :: String -> String
toTitleWord [] = []
toTitleWord (x : xs) = toUpper x : xs
