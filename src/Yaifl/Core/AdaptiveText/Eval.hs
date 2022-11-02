module Yaifl.Core.AdaptiveText.Eval
  ( evaluateAdaptiveText
  , sayAdaptive
  , evalName
  , evalDescription
  ) where

import Yaifl.Core.AdaptiveText
import Solitude
import Effectful
import Yaifl.Core.Say
import Yaifl.Core.Object

evaluateAdaptiveText ::
  AdaptiveText domain
  -> AdaptiveTextDomain domain
  -> Eff es Text
evaluateAdaptiveText = error ""
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