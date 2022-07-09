module Yaifl.Test.Chapter3.Common ( spec ) where

import Yaifl.Test.Common
import Solitude
import Yaifl.Test.Chapter3.Bic
import Test.Syd
--import Yaifl.Core.Test.Chapter3.Verbosity

spec :: Spec
spec = describe "Chapter 3" $ do
  it "Bic" $ 
    testHarness "Bic - 3.1.2" ex2World [] ex2Test
{-  it "runs chapter 3.1.3" $ 
    testHarness "Verbosity - 3.1.3" ex3World ex3TestMeWith ex3Test
-}