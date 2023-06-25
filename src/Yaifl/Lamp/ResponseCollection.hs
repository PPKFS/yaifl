
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Lamp.ResponseCollection where

import Yaifl.Core.Responses
import Yaifl.Core.Object
import Solitude
import Yaifl.Lamp.Say
import Yaifl.Lamp.Actions.Looking
import Yaifl.Lamp.Activities.PrintingTheLocaleDescription
import Yaifl.Lamp.Visibility

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