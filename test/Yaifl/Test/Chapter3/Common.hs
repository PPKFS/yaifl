module Yaifl.Test.Chapter3.Common where

import Yaifl.Test.Common

import Solitude
import Yaifl.Test.Chapter3.Bic
import Yaifl.Test.Chapter3.PortRoyal
import Yaifl.Test.Chapter3.PortRoyal2
import Yaifl.Test.Chapter3.SlightlyWrong
import Yaifl.Test.Chapter3.StarryVoid
import Yaifl.Test.Chapter3.UpAndUp
import Yaifl.Test.Chapter3.Verbosity
import qualified Data.Map as M

spec :: Bool -> Map String (IO Text)
spec allTenses = M.fromList
  [ ("Bic", testHarness allTenses "Bic" [] defaultOptions ex2World)
  , ("Verbosity", testHarness allTenses "Verbosity" ex3TestMeWith defaultOptions ex3World)
  , ("Slightly Wrong", testHarness allTenses "Slightly Wrong" ex4TestMeWith defaultOptions ex4World)
  , ("Port Royal", testHarness allTenses "Port Royal" portRoyalTestMeWith defaultOptions portRoyalWorld)
  , ("Port Royal 2", testHarness allTenses "Port Royal 2" portRoyal2TestMeWith defaultOptions portRoyalWorld2)
  , ("Up and Up", testHarness allTenses "Up and Up" upAndUpTestMeWith defaultOptions upAndUp)
  , ("Starry Void", testHarness allTenses "Starry Void" starryVoidTestMeWith defaultOptions starryVoidWorld)
  ]
