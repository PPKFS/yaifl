
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Lamp.ResponseCollection where

import Solitude
import Yaifl.Lamp.Say
import Yaifl.Lamp.Actions.Looking
import Yaifl.Lamp.Activities.PrintingTheLocaleDescription

data ResponseCollection wm = ResponseCollection
  { roomDescriptions :: RoomDescriptionResponses wm
  , youCanAlsoSee :: YouCanAlsoSeeResponses wm
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

blankResponseCollection :: WithPrintingNameOfSomething wm => ResponseCollection wm
blankResponseCollection = ResponseCollection
  { roomDescriptions = roomDescriptionResponsesImpl
  , youCanAlsoSee = youCanAlsoSeeResponsesImpl
  }