{-|
Module      : Yaifl.Actions.Args
Copyright   : (c) Avery 2024-2025
License     : MIT
Maintainer  : ppkfs@outlook.com

Action argument handling and configuration.

This module defines the data structures and typeclasses for handling action
arguments in Yaifl. It provides the infrastructure for:

- **Action arguments**: Payload types containing source, variables, and configuration
- **Argument parsing**: Two-phase parsing from raw input to typed variables
- **Main object extraction**: Unified interface for accessing primary action targets
- **Action configuration**: Silent vs normal action behavior

The `Args` type is fundamental to Yaifl's action system, serving as the standard
payload for all actions. It includes:

- **Source**: The entity performing the action (player, NPC, etc.)
- **Variables**: Action-specific parameters (targets, tools, etc.)
- **Options**: Configuration like prompt suppression
- **Timestamp**: When the action was initiated

Key features:
- Two-phase parsing for flexible argument handling
- Type-safe variable extraction via `ActionExpects`
- Main object abstraction for generic preconditions
- Silent action support for UI flexibility

Example usage:
@
  -- Create action arguments
  let args = Args
        { source = player
        , variables = targetObject
        , actionOptions = normalAction
        , timestamp = currentTime
        }
  
  -- Access the main object
  mainObj <- args ^. argsMainObject
  
  -- Check if action is silent
  unlessSilent args $ print "Action performed!"
@

This module works closely with:
- `Yaifl.Actions.GoesWith`: Action parameter parsing
- `Yaifl.Action`: Action definitions and execution
- `Yaifl.Core.Rules`: Rulebook system for action processing
-}

{-# LANGUAGE RecordWildCards #-}

module Yaifl.Actions.Args
  ( -- * Action arguments
    Args(..)
  , UnverifiedArgs(..)
  , ActionOptions(..)
  , unlessSilent
  -- ** Argument variables with notions of "main" objects
  -- Often it makes sense to talk about the main object in some action's variables - for example, the
  -- room a player is going to or the object they wish to use. These two typeclasses allow us to write general
  -- preconditions for 'Yaifl.Core.Rules.Rule' - we don't need to write separate helpers for preconditions of
  -- open <specific object>, for examining <specific object>, etc all specialised to the various variable types of actions;
  --  both of these implement `ArgsHaveMainObject` and the preconditions can be written as @[theObject <specific object>]@ accordingly.
  , ArgsHaveMainObject(..)
  , ArgsMightHaveMainObject(..)
  -- * Standard action configurations
  , silentAction
  , normalAction
  ) where

import Yaifl.Prelude hiding (show)

import Yaifl.Object.Kind
import Yaifl.Actions.GoesWith
import Yaifl.Thing.Kind
import Yaifl.Refreshable
import Yaifl.WorldModel
import Yaifl.Effects.ActionHandler
import Yaifl.Room.Kind

-- | Standard action argument payload.
--
-- This record contains all the information needed to execute an action in Yaifl.
-- It serves as the standard argument type for all actions, providing:
--
-- - `source`: The entity performing the action (typically the player)
-- - `variables`: Action-specific parameters (targets, tools, etc.)
-- - `actionOptions`: Configuration for this action invocation
-- - `timestamp`: When the action was initiated
--
-- The `Args` type is parameterized by:
-- - `wm`: The world model type
-- - `v`: The type of action-specific variables
--
-- Example:
-- @
--   -- Args for a "take" action
--   data TakeVariables wm = TakeVariables { target :: Thing wm }
--   
--   takeArgs = Args
--     { source = player
--     , variables = TakeVariables { target = sword }
--     , actionOptions = normalAction
--     , timestamp = currentTime
--     }
-- @
--
-- This type is used throughout Yaifl's action system as the standard way to
-- pass action context and parameters.
data Args (wm :: WorldModel) v = Args
  { source :: Thing wm -- ^ the originating entity that is performing the action.
  , variables :: v -- ^ the variables for the action.
  , actionOptions :: ActionOptions wm -- ^ specific configuration for this invokation of the action.
  , timestamp :: Timestamp -- ^ the timestamp when the action was initiated.
  } deriving stock (Eq, Ord, Generic)

instance (Display v, Display (WMText wm)) => Display (Args wm v) where
  displayBuilder Args{..} = mconcat
    [ "Action arguments: \n"
    , "Source: "
    , displayBuilder source
    , "\nVariables: "
    , displayBuilder variables
    ]

-- | Refresh an `Args` value by refreshing its components.
--
-- This instance enables the `Args` type to work with Yaifl's refreshable system,
-- which is used for updating stale references after object modifications.
--
-- The implementation refreshes both the source entity and the variables,
-- ensuring all references remain valid after world modifications.
--
-- Example:
-- @
--   -- Refresh args after object updates
--   freshArgs <- refresh oldArgs
-- @
instance {-# OVERLAPPING #-} Refreshable wm v => Refreshable wm (Args wm v) where
  refresh av = do
    v <- refresh (variables av)
    o <- refresh $ source av
    return $ av { source = o, variables = v }

-- | Argument parsing is done in 2 steps. When a verb is identified in a command and the specific action is found,
-- the rest of the command is matched against the type of parameters the action expects (its 'Yaifl.Actions.GoesWith' parameter) as well
-- as if it matches any keywords. For instance, the command "USE KEY WITH LOCK" will produce an 'UnverifiedArgs' where the first element is
-- @someKey :: Thing wm@ (whatever the word "key" is matched to) and a matches with pair of @[("with", someLock :: Thing wm)]@.
-- This is after typechecking has occured (meaning "GO KEY" will not get this far) but before the arguments are parsed into a nice form -
-- for the example of a use action, this would be something like @data UsingVariables wm = Using { mainObject :: Thing wm, usingWith :: Maybe (Thing wm)}@.
--
-- for converting 'UnverifiedArgs to 'v', look at 'Yaifl.Action.Action.parseArguments'.
newtype UnverifiedArgs wm (goesWith :: ActionSignature) = UnverifiedArgs
  { unArgs :: Args wm (ActionExpects wm goesWith, [(Text, ActionParameter wm)])
  } deriving newtype (Generic)

makeFieldLabelsNoPrefix ''Args
makeFieldLabelsNoPrefix ''UnverifiedArgs

instance Functor (Args wm) where
  fmap f = #variables %~ f

-- | Action configuration options.
--
-- This data type controls how actions are executed and displayed.
-- Currently supports:
-- - `silent`: Whether to suppress the command prompt
-- - `suppressResponses`: Whether to suppress action responses
--
-- Example:
-- @
--   -- Create silent action configuration
--   config = ActionOptions True True
--   
--   -- Create normal action configuration
--   config = ActionOptions False False
-- @
--
-- This type enables fine-grained control over action execution behavior,
-- particularly useful for NPC actions or system-generated actions that
-- shouldn't produce player-visible output.
data ActionOptions wm = ActionOptions
  { silently :: Bool -- ^ Suppress the command prompt (e.g., ">")
  , suppressResponses :: Bool -- ^ Suppress action responses and output
  }

-- | Default configuration for silent actions.
--
-- Use this configuration when you want an action to execute without any
-- visible output or prompts. This is useful for:
-- - NPC actions
-- - System-generated actions
-- - Background processes
-- - Actions that should be invisible to the player
--
-- Example:
-- @
--   -- Perform an action silently
--   args = Args { actionOptions = silentAction, ... }
-- @
silentAction :: ActionOptions wm
silentAction = ActionOptions True True

-- | Default configuration for normal actions.
--
-- Use this configuration for standard player actions that should:
-- - Show the command prompt
-- - Display all action responses
-- - Provide full feedback to the player
--
-- Example:
-- @
--   -- Perform a normal player action
--   args = Args { actionOptions = normalAction, ... }
-- @
normalAction :: ActionOptions wm
normalAction = ActionOptions False False

-- | Execute an action only if it's not silent.
--
-- This utility function checks if an action is configured to be silent
-- and only executes the given computation if it's not.
--
-- Parameters:
-- - `args`: Action arguments to check
-- - `computation`: Action to perform if not silent
--
-- Returns: Unit result
--
-- Example:
-- @
--   -- Print a message only for non-silent actions
--   unlessSilent args $ print "Action performed!"
--   
--   -- Show UI effects for visible actions
--   unlessSilent args showSpecialEffects
-- @
--
-- This is commonly used for UI elements, sound effects, and other
-- player-visible aspects that should be suppressed for silent actions.
unlessSilent ::
  Applicative m
  => Args wm v -- ^ Action arguments to check
  -> m () -- ^ Computation to perform if not silent
  -> m ()
unlessSilent args = unless (silently . actionOptions $ args)

-- | Unifying typeclass for getting some sort of main object out of something. This is lawless (except normal lens laws),
-- but it should be the...well, something that can be seen as the main object. Examples would be the object being used (but not the one
-- being used *with*) or the room being travelled to (but not the door gone through).
class ArgsHaveMainObject argVars obj | argVars -> obj where
  argsMainObject :: Lens' argVars obj

-- | Unifying typeclass for MAYBE getting some sort of main object out of something. Used for actions where the object may or may not exist
-- (for example, it's possible to examine a direction. For some reason. This means examining variables do not have a main object every time, but
-- only most of the time.)
class ArgsMightHaveMainObject argVars obj | argVars -> obj where
  argsMainObjectMaybe :: AffineTraversal' argVars obj

instance ArgsHaveMainObject (Thing wm) (Thing wm) where
  argsMainObject = castOptic $ iso id id

instance ArgsHaveMainObject (Room wm) (Room wm) where
  argsMainObject = castOptic $ iso id id

instance (ArgsHaveMainObject vars o) => ArgsHaveMainObject (Args wm vars) o where
  argsMainObject = #variables % argsMainObject

instance (ArgsHaveMainObject vars o) => ArgsMightHaveMainObject (Args wm vars) o where
  argsMainObjectMaybe = #variables % argsMainObjectMaybe

instance {-# OVERLAPS #-} (ArgsHaveMainObject vars o) => ArgsMightHaveMainObject vars o where
  argsMainObjectMaybe = castOptic argsMainObject
