module Yaifl.Test.Chapter3.Common ( examples, goldens ) where

import Yaifl.Test.Common

import Yaifl.Test.Chapter3.Bic
import Yaifl.Test.Chapter3.Verbosity
--import Yaifl.Core.Test.Chapter3.Verbosity

examples :: Map String (IO Text)
examples = fromList
  [ ("Bic", testHarness "Bic" ex2World [])
  , ("Verbosity", testHarness "Bic" ex3World ex3TestMeWith)
  ]

goldens :: Map String [Text]
goldens = fromList
  [ ("Bic", ex2Test)
  , ("Verbosity", ex3Test)
  ]
{-  it "runs chapter 3.1.3" $ 
    testHarness "Verbosity - 3.1.3" ex3World ex3TestMeWith ex3Test
-}