{-# LANGUAGE Strict #-}

module Main ( main ) where

import Yaifl.Prelude

import Data.Aeson
import Data.List (lookup)
import Breadcrumbs
import System.Directory
import Test.Tasty hiding (defaultMain)
import Test.Tasty.Silver
import Test.Tasty.Options
import Text.Printf (printf)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Yaifl.Zork.World
import Yaifl.Text.Verb
import Test.Tasty.Silver.Interactive (defaultMain)
import Yaifl.Test.Common
import Yaifl.Zork.World.House (testMeWith)
import Yaifl.Zork.World (defaultZorkOptions, zorkWorld)
import Test.Tasty (testGroup)

-- this is a rip of tasty's main, but hooking my own global `TraceID` through it for
-- better Zipkin traces.
main :: IO ()
main = runEff
  . runBreadcrumbs Nothing $
    do
      testTree <- liftIO goldenTests
      runNo <- liftIO getAndIncrementRunNumber
      withSpan' "Test Suite" ("Run #" <> show runNo) $ do
        (TraceID s) <- getTraceId
        liftIO $ do
          writeFileBS "traceid.temp" s
          defaultMain testTree
          removeFile "traceid.temp"
        flush

getAndIncrementRunNumber :: IO Int
getAndIncrementRunNumber = do
  ex <- doesFileExist "run_no"
  (fc :: Maybe Int) <- (if ex then decodeFileStrict "run_no" else pure Nothing)
  let fc' = fromMaybe 1 fc
  encodeFile "run_no" (fc' + 1)
  pure fc'

unionsWithPrefixes ::
  [(String, Map String v)]
  -> Map String v
unionsWithPrefixes = M.unions . map (\(k, v) -> M.mapKeys (\k1 -> k <> "/" <> k1) v)

-- | Create per-action test for Zork
zorkPerActionTests :: IO TestTree
zorkPerActionTests = do
  allPairs <- testHarnessPerAction False "Zork" testMeWith defaultZorkOptions zorkWorld
  let createTest (idx :: Int) (cmd, output) =
        goldenVsAction
          ("Zork: " <> if T.null cmd then "initial" else T.unpack cmd)
          ("test/testcases/Zork-per-action/" <> printf "%02d" (idx :: Int) <> (if T.null cmd then "_init" else "_" <> T.unpack (T.replace " " "_" cmd)) <> ".golden")
          (pure output)
          id
  return $ testGroup "Zork (per-action)" (map (uncurry createTest) (zip ([0..] :: [Int]) allPairs))

goldenTests :: IO TestTree
goldenTests = do
  zorkPerAction <- zorkPerActionTests
  return $ testGroup "Tests" [
      testGroup "Examples" $ map snd $ M.toAscList $ M.mapWithKey (\k v -> goldenVsAction
      ((\(x, y) -> x <> "-" <> y) . second (drop 1) . span (/= '/') $ k) -- test name
      ("test/testcases/" <> k) -- golden file path
      v  -- action whose result is tested
      id) (M.fromList [("Zork", testHarness False "Zork" testMeWith defaultZorkOptions zorkWorld)])
    , zorkPerAction
    ]
