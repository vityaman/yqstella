module Golden (main) where

import Control.Monad (filterM)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (toUpper)
import Data.List (sort)
import qualified Diagnostic.Core as Diagnostic
import qualified Lib as Stella
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Regex.TDFA (AllTextMatches (..), getAllTextMatches, (=~))

newtype Input = Input String

data Output = Output
  { diagnostics :: String,
    areTypesCorrect :: Bool
  }

test :: Input -> Output
test (Input source) =
  let project = Stella.build (Stella.Source source)
   in Output
        { diagnostics = Diagnostic.displays $ Stella.diagnostics project,
          areTypesCorrect = Stella.areTypesCorrect project
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

  source <- readFile (casePath </> "input.yqst")
  let input = Input source

  let Output
        { diagnostics = diagnostics',
          areTypesCorrect = areTypesCorrect'
        } = test input

  let artifact fileName content =
        let name = toTitleWord $ dropExtension $ takeFileName fileName
         in goldenVsStringDiff name diff (casePath </> fileName) (pure $ pack content)

  let nikolai =
        testCase "Nikolai Kudasov" $ do
          (exitCode, stdout, _) <-
            readProcessWithExitCode "docker" ["run", "-i", "fizruk/stella", "typecheck"] source

          let yqStellaCodes = parseErrorCodes diagnostics'
              fizrukStellaCodes = parseErrorCodes stdout
              isFizrukCodePresent =
                any (`elem` yqStellaCodes) fizrukStellaCodes
                  || null fizrukStellaCodes

          let areFizrukTypesCorrect' = exitCode == ExitSuccess

          assertEqual "yqstella vs fizruk stella status" areFizrukTypesCorrect' areTypesCorrect'
          assertBool
            ("yqstella does not conform to fizruk diagnostics: " ++ stdout)
            isFizrukCodePresent

  let artifacts =
        [ artifact "diagnostics.txt" diagnostics',
          nikolai
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

parseErrorCodes :: String -> [String]
parseErrorCodes t =
  getAllTextMatches (t =~ "ERROR_[A-Z][A-Z0-9_]*" :: AllTextMatches [] String)
