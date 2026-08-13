module Yaifl.Test.Chapter3 where

import Yaifl.Test.Common

import Yaifl.Prelude
import Data.List (lookup)
import Text.Printf (printf)
import Yaifl.Chapter3.Bic
import Yaifl.Chapter3.PortRoyal
import Yaifl.Chapter3.PortRoyal2
import Yaifl.Chapter3.PortRoyal3
import Yaifl.Chapter3.SlightlyWrong
import Yaifl.Chapter3.StarryVoid
import Yaifl.Chapter3.UpAndUp
import Yaifl.Chapter3.Verbosity
import Yaifl.Chapter3.TheUnbuttonedElevatorAffair
import Yaifl.Chapter3.DisenchantmentBay
import Yaifl.Chapter3.DisenchantmentBay2
import qualified Data.Map as M
import Yaifl (PlainWorldModel, Game)
import Yaifl.Chapter3.FirstNameBasis
import Yaifl.Chapter3.MidsummerDay
import Yaifl.Chapter3.Tamed
import Yaifl.Chapter3.Replanting
import Yaifl.Chapter3.DisenchantmentBay4
import Yaifl.Chapter3.Laura
import Yaifl.Chapter3.Escape
import Yaifl.Chapter3.Garibaldi (ex22)
import Yaifl.Run
import Yaifl.Effects.Interpreters
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Silver (goldenVsAction)
import qualified Data.Text as T

c3Harness :: (Text, [Text], WorldConstruction PlainWorldModel ()) -> (String, IO Text)
c3Harness (n, ac, g) = (toString n, testHarness False n ac defaultOptions g)

-- | All Chapter 3 examples
allExamples :: [(Text, [Text], WorldConstruction PlainWorldModel ())]
allExamples = [
    ex2,
    ex3,
    ex4,
    ex5,
    ex6,
    ex7,
    ex8,
    ex9,
    ex10,
    ex11,
    ex12,
    ex13,
    ex14,
    ex15,
    ex16,
    ex18,
    ex19,
    ex21,
    ex22
  ]

spec :: Bool -> Map String (IO Text)
spec _allTenses = M.fromList
  [ -- example 1 (1.1) is just an explanation of the examples' documentation.
    c3Harness ex2 -- bic
  , c3Harness ex3 -- verbosity
  , c3Harness ex4 -- slightly wrong
  , c3Harness ex5 -- Port Royal
  , c3Harness ex6 -- up and up
  , c3Harness ex7 -- starry void
  , c3Harness ex8 -- Port Royal 2
  , c3Harness ex9 -- unbuttoned elevator affair
  , c3Harness ex10 -- Port Royal 3
  , c3Harness ex11 -- First Name Basis
  , c3Harness ex12 -- Midsummer Day
  , c3Harness ex13 -- Tamed
  , c3Harness ex14 -- Disenchantment Bay
  , c3Harness ex15 -- Disenchantment Bay 2
  , c3Harness ex16 -- Replanting
  -- example 17 is a snippet about backdrops, which is used in example 18.
  , c3Harness ex18 -- Disenchantment Bay 4
  , c3Harness ex19 -- Laura
  -- example 20 is a single line of disenchantment bay
  , c3Harness ex21 -- Escape
  , c3Harness ex22 -- Garibaldi
  ]

-- | Per-action test spec
perActionSpec :: IO TestTree
perActionSpec = do
  tests <- mapM (\example -> do
      let (name, commands, worldConstruction) = example
      pairs <- testHarnessPerAction False name commands defaultOptions worldConstruction
      let createTest (idx :: Int) (cmd, output) = 
            goldenVsAction
              (toString name <> ": " <> if T.null cmd then "initial" else toString cmd)
              ("test/testcases/Chapter3-per-action/" <> T.unpack (T.replace " " "_" name) <> "/" <> printf "%02d" (idx :: Int) <> (if T.null cmd then "_init" else "_" <> T.unpack (T.replace " " "_" cmd)) <> ".golden")
              (pure output)
              id
      return $ testGroup (toString name) (map (uncurry createTest) (zip ([0..] :: [Int]) pairs))
    ) allExamples
  return $ testGroup "Chapter3 (per-action)" tests
