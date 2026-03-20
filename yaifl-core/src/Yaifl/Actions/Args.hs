{-|
Module      : Yaifl.Actions.Args
Copyright   : (c) Avery 2024-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Action argument handling and configuration.

Provides:
- `Args`: Standard action argument payload
- `UnverifiedArgs`: Intermediate parsing stage
- Typeclasses for main object extraction
- Silent/normal action configuration
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
data Args (wm :: WorldModel) v = Args
  { source :: Thing wm -- ^ the originating object that is performing the action.
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
-- Refreshes both source entity and variables to maintain valid references.
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

-- | Default configuration for silent actions (no output/prompts).
silentAction :: ActionOptions wm
silentAction = ActionOptions True True

-- | Default configuration for normal actions (full output/feedback).
normalAction :: ActionOptions wm
normalAction = ActionOptions False False

-- | Execute an action only if it's not silent.
--
-- Used for action printing that should be suppressed for silent actions.
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
  -- | Delegate to variables' main object through the variables lens.
  argsMainObject = #variables % argsMainObject

instance (ArgsHaveMainObject vars o) => ArgsMightHaveMainObject (Args wm vars) o where
  -- | Delegate to variables' main object (maybe) through the variables lens.
  argsMainObjectMaybe = #variables % argsMainObjectMaybe

instance {-# OVERLAPS #-} (ArgsHaveMainObject vars o) => ArgsMightHaveMainObject vars o where
  argsMainObjectMaybe = castOptic argsMainObject
