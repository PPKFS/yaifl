{-|
Module      : Yaifl.Effects.ActionHandler
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Action handling effect for parsing and executing player actions.

Provides the 'ActionHandler' effect for parsing player input into structured
actions and handling their execution. Supports configurable execution options
and integration with the game's action system.
-}

module Yaifl.Effects.ActionHandler
  ( ActionHandler(..)
  , ActionOptions(..)
  , parseAction
  ) where

import Yaifl.Prelude

import Yaifl.Actions.GoesWith
import Effectful.TH ( makeEffect )

-- | Configuration options for action execution.
data ActionOptions wm = ActionOptions
  { silently :: Bool -- ^ Whether routine messages should be printed (e.g. silently taking will not produce output). This does not affect failure messages.
  , hidePrompt :: Bool -- ^ Whether the prompt should be shown for this action.
  } deriving stock (Eq, Ord, Show, Read, Generic)

-- | The 'ActionHandler' effect provides functionality for parsing and executing actions.
data ActionHandler wm :: Effect where
  -- | Parse a text command into an action and execute it.
  ParseAction :: ActionOptions wm -> [ActionParameter wm] -> Text -> ActionHandler wm m (Either Text Bool)

makeEffect ''ActionHandler

makeFieldLabelsNoPrefix ''ActionOptions