module Yaifl.Core.AdaptiveText.Eval
  ( evaluateAdaptiveText
  , sayAdaptive
  , evalName
  , evalDescription
  ) where

import Solitude

import Yaifl.Core.AdaptiveText
import Yaifl.Core.Object
import Yaifl.Core.Say ( Saying, say )

evaluateAdaptiveText ::
  AdaptiveText domain
  -> AdaptiveTextDomain domain
  -> Eff es Text
evaluateAdaptiveText (StaticText t) _ = pure t
evaluateAdaptiveText (AdaptiveText ls rs) _ = error $ "not supporting adaptive text yet" <> show ls <> show rs

sayAdaptive ::
  Saying :> es
  => AdaptiveText domain
  -> AdaptiveTextDomain domain
  -> Eff es ()
sayAdaptive t dom = evaluateAdaptiveText t dom >>= say

evalName ::
  (CanBeAny wm (Object wm d))
  => Object wm d
  -> Eff es Text
evalName o = evaluateAdaptiveText (_objName o) (toAny o)

evalDescription ::
  (CanBeAny wm (Object wm d))
  => Object wm d
  -> Eff es Text
evalDescription o = evaluateAdaptiveText (_objDescription o) (toAny o)