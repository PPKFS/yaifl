{-# LANGUAGE Strict #-}

module Main ( main ) where

import Solitude

import Data.Aeson
import Breadcrumbs
import System.Directory
import Test.Tasty hiding (defaultMain)
import Test.Tasty.Silver
import Test.Tasty.Options
import qualified Data.Map as M
import qualified Yaifl.Test.Chapter3.Common as Chapter3
import Yaifl.Text.Verb
import Test.Tasty.Silver.Interactive (defaultMain)

newtype AllTenses = AllTenses Bool

instance IsOption AllTenses where
  defaultValue = AllTenses False

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

makeExampleMap :: Bool -> Map String (IO Text)
makeExampleMap allTenses = unionsWithPrefixes [
  ("Chapter3", Chapter3.spec allTenses)
  ]

makeVerbs :: Map String (IO Text)
makeVerbs = M.fromList $ fmapToSnd (pure . makeVerbCase) ["eat", "have", "be"]

makeVerbCase :: String -> Text
makeVerbCase s =
  let
    forUniverse :: (Bounded a, Enum a, Monoid b) => (a -> b) -> b
    forUniverse = mconcat . flip map universe
  {-mconcat $ mconcat $ mconcat $ mconcat $
    forUniverse @Voice $ \v ->


     -> -}
    v = Active
    vs = Negative
   in
    forUniverse @VerbSense $ \_vs ->
    forUniverse @Tense $ \t ->
    forUniverse @VerbPersonage $ \vp ->
      mconcat $ ["Voice:", show v, " Tense: ", show t, " Sense: ", show vs, "| ",
      case vp of
        FirstPersonSingular -> "I "
        FirstPersonPlural -> "We "
        SecondPersonPlural -> "You "
        SecondPersonSingular -> "You "
        ThirdPersonSingular -> "Alice "
        ThirdPersonPlural -> "Alice and Steve "
      ,
        (\(Verb _ tab) -> runTabulation tab v t vs vp) $ makeVerb (toText s), " Bob.\n"]


unionsWithPrefixes ::
  [(String, Map String v)]
  -> Map String v
unionsWithPrefixes = M.unions . map (\(k, v) -> M.mapKeys (\k1 -> k <> "/" <> k1) v)

goldenTests :: IO TestTree
goldenTests = do
  let allExamples = makeExampleMap
      _allVerbs = makeVerbs
  return $ askOption $ \(AllTenses a) ->
    testGroup "Tests" [
      testGroup "Examples" $ map snd $ M.toAscList $ M.mapWithKey (\k v -> goldenVsAction
      ((\(x, y) -> x <> "-" <> y) . second (drop 1) . span (/= '/') $ k) -- test name
      ("test/testcases/" <> k) -- golden file path
      v  -- action whose result is tested
      id)
      (allExamples a)
    {- }, testGroup "Conjugation" $ map snd $ M.toAscList $ M.mapWithKey (\k v -> goldenVsStringDiff
      k -- test name
      (\ref new -> ["delta", ref, new])

      ("test/testcases/verbs/" <> k) -- golden file path
      (encodeUtf8 <$> v))  -- action whose result is tested
      allVerbs

      --map doConjugation ["have", "be", "see", "eat", "fall"]-}

    ]
