module Yaifl.Test.Chapter3.Common ( spec ) where

import Yaifl.Test.Common

import Yaifl.Test.Chapter3.Bic
import Yaifl.Test.Chapter3.Verbosity
import Test.Sandwich
import Control.Monad.Catch
--import Yaifl.Core.Test.Chapter3.Verbosity

spec :: (MonadIO m, MonadMask m, HasBaseContext context) => SpecFree context m ()
spec = describe "Chapter 3" $ do
  testHarness "Bic" ex2World [] ex2Test
  testHarness "Verbosity" ex3World ex3TestMeWith ex3Test

{-  it "runs chapter 3.1.3" $ 
    testHarness "Verbosity - 3.1.3" ex3World ex3TestMeWith ex3Test
-}