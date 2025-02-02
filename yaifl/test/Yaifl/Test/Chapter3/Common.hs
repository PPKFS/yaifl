module Yaifl.Test.Chapter3.Common where

import Yaifl.Test.Common

import Yaifl.Prelude
import Yaifl.Test.Chapter3.Bic
import Yaifl.Test.Chapter3.PortRoyal
import Yaifl.Test.Chapter3.PortRoyal2
import Yaifl.Test.Chapter3.PortRoyal3
import Yaifl.Test.Chapter3.SlightlyWrong
import Yaifl.Test.Chapter3.StarryVoid
import Yaifl.Test.Chapter3.UpAndUp
import Yaifl.Test.Chapter3.Verbosity
import Yaifl.Test.Chapter3.TheUnbuttonedElevatorAffair
import Yaifl.Test.Chapter3.DisenchantmentBay
import Yaifl.Test.Chapter3.DisenchantmentBay2
import qualified Data.Map as M
import Yaifl (PlainWorldModel, Game)
import Yaifl.Test.Chapter3.FirstNameBasis
import Yaifl.Test.Chapter3.MidsummerDay
import Yaifl.Test.Chapter3.Tamed
import Yaifl.Test.Chapter3.Replanting
import Yaifl.Test.Chapter3.DisenchantmentBay4
import Yaifl.Test.Chapter3.Laura
import Yaifl.Test.Chapter3.Escape

c3Harness :: (Text, [Text], Game PlainWorldModel ()) -> (String, IO Text)
c3Harness (n, ac, g) = (toString n, testHarness False n ac defaultOptions g)

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
  , c3Harness ex21
  ]
