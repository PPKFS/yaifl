{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.Responses where

import Yaifl.Core.Rulebooks.Rule

import Solitude
import Yaifl.Core.Object


data ResponseCollection wm = ResponseCollection
  { roomDescriptionHeadingA :: Response wm ()
  , roomDescriptionHeadingB :: Response wm (AnyObject wm)
  , roomDescriptionHeadingC :: Response wm (AnyObject wm)
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

blankResponseCollection :: ResponseCollection wm
blankResponseCollection = ResponseCollection
  { roomDescriptionHeadingA = Response $ const $ pure "Darkness"
  , roomDescriptionHeadingB = Response $ const $ pure "Darkness"
  , roomDescriptionHeadingC = Response $ const $ pure "Darkness"
  }