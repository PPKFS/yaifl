-- | Action handling effect for parsing and executing player actions.
--
-- This module provides the 'ActionHandler' effect which is responsible for
-- parsing player input into structured actions and handling the execution
-- of those actions within the game world.
--
-- The system supports:
-- * Parsing natural language commands into action structures
-- * Configurable action execution options
-- * Integration with the game's action system
--
-- Example usage:
--
-- @
-- -- Parse and execute a player command
-- result <- parseAction defaultOptions [player] "take sword"
-- case result of
--   Left errorMsg -> say $ "I didn't understand: " <> errorMsg
--   Right success -> when success $ say "Action completed."
-- @

module Yaifl.Effects.ActionHandler
  ( ActionHandler(..)
  , ActionOptions(..)
  , parseAction
  ) where

import Yaifl.Prelude

import Yaifl.Actions.GoesWith
import Effectful.TH ( makeEffect )

-- | Configuration options for action execution.
--
-- This record contains settings that control how actions are executed
-- and what output they produce. It allows fine-grained control over
-- the verbosity and user interface aspects of action handling.
--
-- Type parameter 'wm' represents the world model being used.
--
-- Example:
--
-- @
-- -- Create options for silent action execution
-- silentOptions = ActionOptions
--   { silently = True
--   , hidePrompt = True
--   }
-- @

data ActionOptions wm = ActionOptions
  { silently :: Bool -- ^ Whether routine messages should be printed (e.g. silently taking will not produce output). This does not affect failure messages.
  , hidePrompt :: Bool -- ^ Whether the prompt should be shown for this action.
  } deriving stock (Eq, Ord, Show, Read, Generic)

-- | The 'ActionHandler' effect provides functionality for parsing and executing actions.
--
-- This effect handles the core interaction loop of parsing player input
-- into structured actions and managing their execution.
--
-- Type parameters:
-- * 'wm': The world model being used
-- * 'm': The monad in which this effect is interpreted

data ActionHandler wm :: Effect where
  -- | Parse a text command into an action and execute it.
  --
  -- Takes action options, a list of action parameters (typically objects involved),
  -- and the raw text command. Returns either an error message or a boolean
  -- indicating success.
  --
  -- Example:
  --
  -- @
  -- -- Parse "take sword" with default options
  -- result <- parseAction defaultOptions [player] "take sword"
  -- @
  ParseAction :: ActionOptions wm -> [ActionParameter wm] -> Text -> ActionHandler wm m (Either Text Bool)

-- | Generate effectful boilerplate for the 'ActionHandler' effect.
-- This creates the necessary typeclass instances and functions.
makeEffect ''ActionHandler

-- | Generate field labels without prefix for the 'ActionOptions' type.
-- This enables using field names directly as lenses.
makeFieldLabelsNoPrefix ''ActionOptions