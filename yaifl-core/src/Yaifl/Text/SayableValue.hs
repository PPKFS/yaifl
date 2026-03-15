{-|
Module      : Yaifl.Text.SayableValue
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Typeclass for values that can be converted to text output.

This module defines the `SayableValue` typeclass which provides the foundation
for the text output system. Types that implement this class can be used in text
generation throughout the game system.

The typeclass provides two methods:
- `sayTell`: Outputs text to a Writer effect for accumulation
- `say`: Outputs text directly using the Print effect

Default instances are provided for `Text` and `String`.
-}

module Yaifl.Text.SayableValue
  ( -- * Core Typeclass
    SayableValue(..)
  )
where

import Yaifl.Prelude
import Yaifl.Effects.RuleEffects
import Effectful.Writer.Static.Local
import Yaifl.Effects.Print

-- | Typeclass for values that can be converted to text output.
--
-- Types implementing this class can be used anywhere text output is generated
-- in the game system. The class provides two methods:
--
-- * `sayTell`: Writes text to a Writer effect where it can be accumulated
-- * `say`: Outputs text directly using the Print effect
--
-- Implementations should provide at least `sayTell`. The default `say` implementation
-- uses `sayTell` and prints the result.
class SayableValue s wm where
    -- | Output text to a Writer effect for accumulation.
  sayTell :: (Writer Text :> es, RuleEffects wm es) => s -> Eff es ()
  
    -- | Output text directly using the Print effect.
  say :: RuleEffects wm es => s -> Eff es ()
  default say :: RuleEffects wm es => s -> Eff es ()
  say s = do
    r <- execWriter (sayTell s)
    when (r /= "") $ printText r

-- | Default instance for `Text` values.
-- Directly tells the text using the Writer effect.
instance SayableValue Text wm where
  sayTell = tell

-- | Instance for `String` values.
-- Converts the string to text before telling it.
instance SayableValue String wm where
  sayTell = tell . toText

-- | Instance for `Int` values.
-- Converts the integer to text before telling it.
instance SayableValue Int wm where
  sayTell = tell . show

-- | Instance for `Double` values.
-- Converts the double to text before telling it.
instance SayableValue Double wm where
  sayTell = tell . show

-- | Instance for `Integer` values.
-- Converts the integer to text before telling it.
instance SayableValue Integer wm where
  sayTell = tell . show

-- | Instance for `Bool` values.
-- Converts the boolean to lowercase "true" or "false" text.
instance SayableValue Bool wm where
  sayTell = tell . bool "false" "true"
