module Yaifl.Text.SayableValue
  ( SayableValue(..)
  ) where

import Yaifl.Prelude
import Yaifl.Effects.RuleEffects
import Effectful.Writer.Static.Local
import Yaifl.Effects.Print

class SayableValue s wm where
  sayTell :: (Writer Text :> es, RuleEffects wm es) => s -> Eff es ()
  say :: RuleEffects wm es => s -> Eff es ()
  default say :: RuleEffects wm es => s -> Eff es ()
  say s = do
    r <- execWriter (sayTell s)
    when (r /= "") $ printText r

instance SayableValue Text wm where
  sayTell = tell

instance SayableValue String wm where
  sayTell = tell . toText
