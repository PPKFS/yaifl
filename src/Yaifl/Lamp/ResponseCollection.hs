
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Lamp.ResponseCollection where

import Yaifl.Core.Responses
import Yaifl.Core.Object
import Solitude
import Yaifl.Lamp.Say
import Yaifl.Core.SayQQ
import Yaifl.Core.Rules.RuleEffects

data ResponseCollection wm = ResponseCollection
  { roomDescriptionHeadingA :: Response wm ()
  , roomDescriptionHeadingB :: Response wm (AnyObject wm)
  , roomDescriptionHeadingC :: Response wm (AnyObject wm)
  , roomDescriptionBodyA :: Response wm ()
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

blankResponseCollection :: WithPrintingNameOfSomething wm => ResponseCollection wm
blankResponseCollection = ResponseCollection
  { roomDescriptionHeadingA = Response $ const [sayingTell|Darkness|]
  , roomDescriptionHeadingB = Response $ \intermediateLevel -> do
      [sayingTell|(on {The intermediateLevel})|]
      sayTell @Text "(on"
      sayTell $ The_ intermediateLevel
      sayTell @Text ")"
  , roomDescriptionHeadingC = Response $ \intermediateLevel -> do
      sayTell @Text "(in"
      sayTell $ The_ intermediateLevel
      sayTell @Text ")"
  , roomDescriptionBodyA = Response $ const $ do
      pass
      {- sayTell It
      sayTell $ Verb_ Be
      sayTell "pitch dark, and "
      sayTell We_
      sayTell $ Can't_ (Verb_ See)
      sayTell " a thing"
      -}
  }