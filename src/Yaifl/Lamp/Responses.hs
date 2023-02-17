{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.Responses where

import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Print
import Solitude


data ResponseCollection wm = ResponseCollection
  { nameOfADarkRoomA :: Response wm
  , v :: ()
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

blankResponseCollection :: ResponseCollection wm
blankResponseCollection = ResponseCollection
  { nameOfADarkRoomA = Response $ printText "Darkness"
  , v = ()
  }