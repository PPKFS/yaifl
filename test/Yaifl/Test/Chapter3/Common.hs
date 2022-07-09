module Yaifl.Test.Chapter3.Common ( examples, goldens ) where

import Yaifl.Test.Common

import Yaifl.Test.Chapter3.Bic
import Test.Syd
--import Yaifl.Core.Test.Chapter3.Verbosity

examples :: Map String (IO Text)
examples = fromList
  [ ("Bic", testHarness "Bic" ex2World [])
  ]

goldens :: Map String [Text]
goldens = fromList
  [ ("Bic", ex2Test)
  ]
{-  it "runs chapter 3.1.3" $ 
    testHarness "Verbosity - 3.1.3" ex3World ex3TestMeWith ex3Test
-}