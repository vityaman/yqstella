module Golden (main) where

import Control.Monad (filterM)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (toUpper)
import Data.List (sort)
import Diagnostic.Core (display)
import qualified Lib as Stella
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Regex.TDFA (AllTextMatches (..), getAllTextMatches, (=~))
import YQL.PrettyPrint (displayYQL)

data Output = Output
  { diagnostics :: String,
    areTypesCorrect :: Bool,
    yql :: String
  }

test :: Stella.Source -> Output
test source =
  let Stella.Project
        { Stella.diagnostics = diagnostics',
          Stella.areTypesCorrect = areTypesCorrect',
          Stella.yql = yql'
        } = Stella.build source
   in Output
        { diagnostics = display diagnostics',
          areTypesCorrect = areTypesCorrect',
          yql = maybe "" displayYQL yql'
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

  let sourcePath = casePath </> "input.yqst"
      programPath = casePath </> "yql.yqls"
      paramsPath = casePath </> "parameters.yson"
      resultPath = casePath </> "result.yson"

  source <- readFile sourcePath
  let input = Stella.Source sourcePath source

  let Output
        { diagnostics = diagnostics',
          areTypesCorrect = areTypesCorrect',
          yql = yql'
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

          assertEqual
            ("fizruk vs yqstella status: \n" ++ stdout ++ "\n\nVS\n\n" ++ diagnostics')
            areFizrukTypesCorrect'
            areTypesCorrect'
          assertBool
            ("yqstella does not conform to fizruk diagnostics: " ++ stdout)
            isFizrukCodePresent

  let minirun =
        let execute = do
              cwd <- getCurrentDirectory
              let dockerArgs = ["--platform", "linux/amd64", "-v", cwd ++ ":/w", "-w", "/w"]
              let minirunArgs = ["--program", programPath, "--params-file", paramsPath, "--print-result"]
              let args = ["run"] ++ dockerArgs ++ ["vityamand/minirun"] ++ minirunArgs

              (exitCode, stdout, stderr) <-
                if areTypesCorrect'
                  then readProcessWithExitCode "docker" args ""
                  else pure (ExitSuccess, "", "")

              return $ stdout ++ (if exitCode == ExitSuccess then "" else stderr)
         in goldenVsStringDiff "Minirun" diff resultPath (fmap pack execute)

  let artifacts =
        [ artifact "diagnostics.txt" diagnostics',
          artifact "yql.yqls" yql',
          nikolai,
          minirun
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
