
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Lamp.ResponseCollection where

import Solitude
import Yaifl.Lamp.Say
import Yaifl.Lamp.Actions.Looking
import Yaifl.Lamp.Activities.PrintingTheLocaleDescription
import Yaifl.Lamp.ListWriter

data ResponseCollection wm = ResponseCollection
  { roomDescriptions :: RoomDescriptionResponses wm
  , youCanAlsoSee :: YouCanAlsoSeeResponses wm
  , listWriterResponses :: ListWriterResponses wm
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

blankResponseCollection :: WithPrintingNameOfSomething wm => ResponseCollection wm
blankResponseCollection = ResponseCollection
  { roomDescriptions = roomDescriptionResponsesImpl
  , youCanAlsoSee = youCanAlsoSeeResponsesImpl
  , listWriterResponses = listWriterResponsesImpl
  }