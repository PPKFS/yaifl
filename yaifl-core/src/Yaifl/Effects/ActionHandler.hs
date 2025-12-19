module Yaifl.Effects.ActionHandler
  ( ActionHandler(..)
  , ActionOptions(..)
  , parseAction
  ) where

import Yaifl.Prelude

import Yaifl.Actions.GoesWith
import Effectful.TH ( makeEffect )

-- | Configuration for carrying out a 'Yaifl.Core.Action.Action'.
data ActionOptions wm = ActionOptions
  { silently :: Bool -- ^ Whether routine messages should be printed (e.g. silently taking will not produce output). This does not affect failure messages.
  , hidePrompt :: Bool -- ^ Whether the prompt should be shown for this action.
  } deriving stock (Eq, Ord, Show, Read, Generic)

data ActionHandler wm :: Effect where
  ParseAction :: ActionOptions wm -> [ActionParameter wm] -> Text -> ActionHandler wm m (Either Text Bool)

makeEffect ''ActionHandler
makeFieldLabelsNoPrefix ''ActionOptions