{-# LANGUAGE Strict #-}

module Main ( main ) where

import Yaifl.Prelude

import Data.Aeson
import Breadcrumbs
import System.Directory
import Test.Tasty hiding (defaultMain)
import Test.Tasty.Silver
import Test.Tasty.Options
import qualified Data.Map as M
import qualified Yaifl.Zork.World
import Yaifl.Text.Verb
import Test.Tasty.Silver.Interactive (defaultMain)
import Yaifl.Test.Common
import Yaifl.Zork.World.House (testMeWith)
import Yaifl.Zork.World (defaultZorkOptions, zorkWorld)

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

goldenTests :: IO TestTree
goldenTests = do
  return $ testGroup "Tests" [
      testGroup "Examples" $ map snd $ M.toAscList $ M.mapWithKey (\k v -> goldenVsAction
      ((\(x, y) -> x <> "-" <> y) . second (drop 1) . span (/= '/') $ k) -- test name
      ("test/testcases/" <> k) -- golden file path
      v  -- action whose result is tested
      id) (M.fromList [("Zork", testHarness False "Zork" testMeWith defaultZorkOptions zorkWorld)])
    ]
