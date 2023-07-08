{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Responses
  ( Response(..)
  , constResponse
  , sayResponse
  , sayTellResponse
  , WithResponseSet
  , WithResponse
  ) where

import Solitude
import Effectful.Writer.Static.Local (Writer, execWriter)
import Yaifl.Core.WorldModel
import Effectful.Optics
import GHC.TypeLits
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Core.SayQQ

newtype Response wm v = Response { runResponse :: forall es. (RuleEffects wm es) => v -> Eff (Writer Text : es) () }

makeFieldLabelsNoPrefix ''Response

constResponse ::
  Text
  -> Response wm a
constResponse t = Response $ const [sayingTell|{t}|]

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

sayTellResponse ::
  forall wm v es.
  RuleEffects wm es
  => Writer Text :> es
  => Lens' (WMResponses wm) (Response wm v)
  -> v
  -> Eff es ()
sayTellResponse aL v = do
  Response t <- use @(ResponseCollector wm) $ #responseCollection % aL
  r <- execWriter $ t v
  sayTell r

type WithResponseSet wm (name :: Symbol) v = LabelOptic' name A_Lens (WMResponses wm) v
type WithResponse wm (name :: Symbol) v = LabelOptic' name A_Lens (WMResponses wm) (Response wm v)
