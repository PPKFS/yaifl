{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.Responses where

import Yaifl.Core.Rulebooks.Rule

import Solitude
import Yaifl.Core.Object
import Effectful.Writer.Static.Local (tell, Writer, execWriter)
import Yaifl.Core.WorldModel
import Effectful.Optics
import GHC.TypeLits
import Yaifl.Lamp.Say

newtype Response wm v = Response { runResponse :: forall es. (RuleEffects wm es) => v -> Eff (Writer Text : es) () }

makeFieldLabelsNoPrefix ''Response

data ResponseCollection wm = ResponseCollection
  { roomDescriptionHeadingA :: Response wm ()
  , roomDescriptionHeadingB :: Response wm (AnyObject wm)
  , roomDescriptionHeadingC :: Response wm (AnyObject wm)
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''ResponseCollection

sayResponse ::
  forall wm v es.
  RuleEffects wm es
  => Lens' (WMResponses wm) (Response wm v)
  -> v
  -> Eff es ()
sayResponse aL v = do
  Response t <- use @(ResponseCollector wm) $ #responseCollection % aL
  r <- execWriter $ t v
  say r

type WithResponse wm (name :: Symbol) v = LabelOptic' name A_Lens (WMResponses wm) (Response wm v)

blankResponseCollection :: WithPrintingNameOfSomething wm => ResponseCollection wm
blankResponseCollection = ResponseCollection
  { roomDescriptionHeadingA = Response $ const $ tell "Darkness"
  , roomDescriptionHeadingB = Response $ \intermediateLevel -> do
      sayTell @Text "(on"
      sayTell $ The_ intermediateLevel
      sayTell @Text ")"
  , roomDescriptionHeadingC = Response $ \intermediateLevel -> do
      sayTell @Text "(in"
      sayTell $ The_ intermediateLevel
      sayTell @Text ")"
  }
