{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Yaifl.Text.DynamicText
  ( DynamicText(..)
  , text
  ) where

import Yaifl.Core.WorldModel
import Yaifl.Prelude
import Yaifl.Model.Rules.Rulebook
import Effectful.Writer.Static.Local (Writer, tell)
import Data.Text.Lazy.Builder (fromText)
import Yaifl.Text.Say
import Yaifl.Model.Rules.RuleEffects

newtype DynamicText (wm :: WorldModel) = DynamicText (Either Text (Text, RuleLimitedEffect wm (Writer Text) ()))

instance Display (DynamicText wm) where
  displayBuilder (DynamicText (Left t)) = fromText t
  displayBuilder (DynamicText (Right (n, _))) = fromText n

instance IsString (DynamicText wm) where
  fromString = DynamicText . Left . toText

instance SayableValue (WMText wm) wm => SayableValue (DynamicText wm) wm where
  sayTell (DynamicText (Left t)) = tell t
  sayTell (DynamicText (Right (_, RuleLimitedEffect e))) = inject e

instance Semigroup (DynamicText wm) where
  (<>) (DynamicText (Left t)) (DynamicText (Left t2)) = (DynamicText (Left (t <> t2)))
  (<>) (DynamicText (Left t)) (DynamicText (Right (n, RuleLimitedEffect r))) = (DynamicText (Right (n, (RuleLimitedEffect $ tell t >> r))))
  (<>) (DynamicText (Right (n, RuleLimitedEffect r))) (DynamicText (Left t2)) = (DynamicText (Right (n, RuleLimitedEffect $ r >> tell t2 )))
  (<>) (DynamicText (Right (n, RuleLimitedEffect r))) (DynamicText (Right (n2, RuleLimitedEffect r2))) = (DynamicText (Right (n <> n2, RuleLimitedEffect $ r >> r2 )))
instance Monoid (DynamicText wm) where
  mempty = DynamicText (Left "")
text ::
  SayableValue (WMText wm) wm
  => Display (WMText wm)
  => Text
  -> Eff (Writer Text : ConcreteRuleStack wm) ()
  -> DynamicText wm
text t f = DynamicText $ Right (t, RuleLimitedEffect f)