{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Yaifl.Text.DynamicText
  ( DynamicText(..)
  , text
  ) where

import Yaifl.Model.WorldModel
import Solitude
import Yaifl.Model.Rules.Rulebook
import Effectful.Writer.Static.Local (Writer, tell)
import Data.Text.Display
import Data.Text.Lazy.Builder (fromText)
import Yaifl.Text.Say
import Yaifl.Model.Rules.RuleEffects

newtype DynamicText (wm :: WorldModel) = DynamicText (Either Text (Text, RuleLimitedEffect wm (Writer Text) ()))

instance Display (DynamicText wm) where
  displayBuilder (DynamicText (Left t)) = fromText t
  displayBuilder (DynamicText (Right (n, _))) = fromText n

instance IsString (DynamicText wm) where
  fromString = DynamicText . Left . toText

instance SayableValue (DynamicText wm) wm where
  sayTell (DynamicText (Left t)) = tell t
  sayTell (DynamicText (Right (_, RuleLimitedEffect e))) = inject e

text ::
  SayableValue (WMSayable wm) wm
  => Display (WMSayable wm)
  => Text
  -> Eff (Writer Text : ConcreteRuleStack wm) ()
  -> DynamicText wm
text t f = DynamicText $ Right (t, RuleLimitedEffect f)