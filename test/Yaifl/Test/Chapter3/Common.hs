module Yaifl.Test.Chapter3.Common where

import Yaifl.Test.Common

import Solitude
import Yaifl.Test.Chapter3.Bic
import Yaifl.Test.Chapter3.Verbosity
import Yaifl.Test.Chapter3.SlightlyWrong
import qualified Data.Map as M

spec :: Bool -> Map String (IO Text)
spec allTenses = M.fromList
  [ ("Bic", testHarness allTenses "Bic" [] defaultOptions ex2World)
  , ("Verbosity", testHarness allTenses "Verbosity" ex3TestMeWith defaultOptions ex3World)
  , ("Slightly Wrong", testHarness allTenses "Slightly Wrong" ex4TestMeWith defaultOptions ex4World)
  ]
