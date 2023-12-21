module Yaifl.Text.DynamicText
  ( DynamicText(..)
  ) where

import Yaifl.Model.WorldModel
import Solitude
import Yaifl.Rules.Rule
import Effectful.Writer.Static.Local (Writer, tell)
import Data.Text.Display
import Data.Text.Lazy.Builder (fromText)
import Yaifl.Text.Say

newtype DynamicText (wm :: WorldModel) = DynamicText (Either Text (Text, RuleLimitedEffect wm (Writer Text) ()))

instance Display (DynamicText wm) where
  displayBuilder (DynamicText (Left t)) = fromText t
  displayBuilder (DynamicText (Right (n, _))) = fromText n

instance IsString (DynamicText wm) where
  fromString = DynamicText . Left . toText

instance SayableValue (DynamicText wm) wm where
  sayTell (DynamicText (Left t)) = tell t
  sayTell (DynamicText (Right (_, RuleLimitedEffect e))) = inject e