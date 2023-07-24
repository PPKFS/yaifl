
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Text.ResponseCollection where

import Solitude
import Yaifl.Text.Say
import Yaifl.Actions.Looking
import Yaifl.Activities.PrintingTheLocaleDescription
import Yaifl.Text.ListWriter

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