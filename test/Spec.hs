{-# LANGUAGE Strict #-}

module Main ( main ) where

import Solitude

import Data.Aeson ( decodeFileStrict, encodeFile )
import Breadcrumbs
import System.Directory
import System.IO ( hPutStrLn )
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Options
import Test.Tasty.Ingredients
import Test.Tasty.Runners
import qualified Data.Map as M
import qualified Yaifl.Test.Chapter3.Common as Chapter3

newtype AllTenses = AllTenses Bool

instance IsOption AllTenses where
  defaultValue = AllTenses True
  parseValue = fmap AllTenses . safeRead
  optionName = return "alltenses"
  optionHelp = return "Run the examples in all tenses and viewpoints"
  {- optionCLParser = AllTenses <$>
    switch
      (  long (untag (optionName :: Tagged AllTenses String))
      <> help (untag (optionHelp :: Tagged AllTenses String))
      )-}
-- this is a rip of tasty's main, but hooking my own global `TraceID` through it for
-- better Zipkin traces.
main :: IO ()
main = runEff
  . runBreadcrumbs Nothing $
    do
      (testTree, opts) <- liftIO $ do
        testTree <- goldenTests
        installSignalHandlers
        opts <- parseOptions (includingOptions [Option (Proxy @AllTenses)] : defaultIngredients) testTree
        pure (testTree, opts)
      case tryIngredients defaultIngredients opts testTree of
        Nothing -> liftIO $ do
          hPutStrLn stderr
            "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
          exitFailure
        Just act -> do
          runNo <- liftIO getAndIncrementRunNumber
          ok <- withSpan' "Test Suite" ("Run #" <> show runNo) $ do
            (TraceID s) <- getTraceId
            liftIO $ do
              writeFileBS "traceid.temp" s
              o <- liftIO act
              removeFile "traceid.temp"
              pure o
          flush
          liftIO $ if ok then exitSuccess else exitFailure

getAndIncrementRunNumber :: IO Int
getAndIncrementRunNumber = do
  ex <- doesFileExist "run_no"
  (fc :: Maybe Int) <- (if ex then decodeFileStrict "run_no" else pure Nothing)
  let fc' = fromMaybe 1 fc
  encodeFile "run_no" (fc' + 1)
  pure fc'

makeExampleMap :: Bool -> Map String (IO Text)
makeExampleMap allTenses = unionsWithPrefixes [
  ("Chapter 3", Chapter3.spec allTenses)
  ]

unionsWithPrefixes ::
  [(String, Map String v)]
  -> Map String v
unionsWithPrefixes = M.unions . map (\(k, v) -> M.mapKeys (\k1 -> k <> "/" <> k1) v)

goldenTests :: IO TestTree
goldenTests = do
  let allExamples = makeExampleMap
  return $ askOption $ \(AllTenses a) ->
    testGroup "Examples" $ map snd $ M.toAscList $ M.mapWithKey (\k v -> goldenVsStringDiff
      k -- test name
      (\ref new -> ["diff", ref, new])
      ("test/testcases/" <> k) -- golden file path
      (encodeUtf8 <$> v))  -- action whose result is tested
      (allExamples a)
