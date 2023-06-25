
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Lamp.ResponseCollection where

import Yaifl.Core.Responses
import Yaifl.Core.Object
import Solitude
import Yaifl.Lamp.Say
import Yaifl.Lamp.Actions.Looking
import Yaifl.Lamp.Activities.PrintingTheLocaleDescription

data ResponseCollection wm = ResponseCollection
  { roomDescriptionHeadingA :: Response wm ()
  , roomDescriptionHeadingB :: Response wm (AnyObject wm)
  , roomDescriptionHeadingC :: Response wm (AnyObject wm)
  , roomDescriptionBodyA :: Response wm ()
  , youCanAlsoSeeA :: Response wm ()
  , youCanAlsoSeeB :: Response wm (AnyObject wm)
  , youCanAlsoSeeC :: Response wm (AnyObject wm)
  , youCanAlsoSeeD :: Response wm ()
  , youCanAlsoSeeE :: Response wm ()
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

blankResponseCollection :: WithPrintingNameOfSomething wm => ResponseCollection wm
blankResponseCollection = ResponseCollection
  { roomDescriptionHeadingA = roomDescriptionHeadingAImpl
  , roomDescriptionHeadingB = roomDescriptionHeadingBImpl
  , roomDescriptionHeadingC = roomDescriptionHeadingCImpl
  , roomDescriptionBodyA = roomDescriptionBodyAImpl
  , youCanAlsoSeeA = youCanAlsoSeeAImpl
  , youCanAlsoSeeB = youCanAlsoSeeBImpl
  , youCanAlsoSeeC = youCanAlsoSeeCImpl
  , youCanAlsoSeeD = youCanAlsoSeeDImpl
  , youCanAlsoSeeE = youCanAlsoSeeEImpl
  }