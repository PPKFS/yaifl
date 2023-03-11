
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Lamp.ResponseCollection where

import Yaifl.Core.Responses
import Yaifl.Core.Object
import Solitude
import Yaifl.Lamp.Say
import Yaifl.Lamp.Actions.Looking

data ResponseCollection wm = ResponseCollection
  { roomDescriptionHeadingA :: Response wm ()
  , roomDescriptionHeadingB :: Response wm (AnyObject wm)
  , roomDescriptionHeadingC :: Response wm (AnyObject wm)
  , roomDescriptionBodyA :: Response wm ()
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

blankResponseCollection :: WithPrintingNameOfSomething wm => ResponseCollection wm
blankResponseCollection = ResponseCollection
  { roomDescriptionHeadingA = roomDescriptionHeadingAImpl
  , roomDescriptionHeadingB = roomDescriptionHeadingBImpl
  , roomDescriptionHeadingC = roomDescriptionHeadingCImpl
  , roomDescriptionBodyA = roomDescriptionBodyAImpl
  }