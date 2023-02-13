module Yaifl.Test.Chapter3.Common where

import Yaifl.Test.Common

import Solitude
import Yaifl.Test.Chapter3.Bic
import Yaifl.Test.Chapter3.Verbosity
import qualified Data.Map as M
--import Yaifl.Core.Test.Chapter3.Verbosity

spec :: Map String (IO Text)
spec = M.fromList
  [ ("Bic", testHarness "Bic" [] defaultOptions ex2World)
  , ("Verbosity", testHarness "Verbosity" ex3TestMeWith defaultOptions ex3World)
  ]

{-  it "runs chapter 3.1.3" $
    testHarness "Verbosity - 3.1.3" ex3World ex3TestMeWith ex3Test
-}