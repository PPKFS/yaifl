{-|
Module: Yaifl.Core.Actions.Args
Description: Arguments handled by actions.
Copyright: (c) Avery 2024-2025
License: MIT
Maintainer: Avery ppkfs@outlook.com

This module implements a payload type for arguments to game actions that includes the source of the action -
the object (usually the player, but can be an NPC or other AI) that attempted to performm the action as well as a timestamp
and additional options such as whether the prompt icon (usually ">") should be printed.

All 'Yaifl.Core.Action's implicitly use 'Args' as the rulebook variables, whereas plain rulebooks and activities may use any @v@.
-}

{-# LANGUAGE RecordWildCards #-}

module Yaifl.Core.Actions.Args
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
  -- * Helper functions
  , getActorLocation
  ) where

import Yaifl.Prelude hiding (show)

import Yaifl.Core.Kinds.Object
import Yaifl.Core.Actions.GoesWith
import Yaifl.Core.Effects
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.Refreshable
import Yaifl.Core.WorldModel

-- | Configuration for carrying out a 'Yaifl.Core.Action.Action'.
data ActionOptions wm = ActionOptions
  { silently :: Bool -- ^ Whether routine messages should be printed (e.g. silently taking will not produce output). This does not affect failure messages.
  , hidePrompt :: Bool -- ^ Whether the prompt should be shown for this action.
  } deriving stock (Eq, Ord, Show, Read, Generic)

-- | Arguments for a 'Yaifl.Core.Action.Action' with variables of type @v@.
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
-- for converting 'UnverifiedArgs to 'v', look at 'Yaifl.Core.Action.Action.parseArguments'.
newtype UnverifiedArgs wm (goesWith :: ActionParameterType) = UnverifiedArgs
  { unArgs :: Args wm (ActionParameter wm goesWith, [(Text, NamedActionParameter wm)])
  } deriving newtype (Generic)

makeFieldLabelsNoPrefix ''Args
makeFieldLabelsNoPrefix ''UnverifiedArgs
makeFieldLabelsNoPrefix ''ActionOptions

instance Functor (Args wm) where
  fmap f = #variables %~ f

-- | Default configuration for silent actions - don't print the prompt, do the action silently.
silentAction :: ActionOptions wm
silentAction = ActionOptions True True

-- | Default configuration for normal actions - print the prompt, do not suppress any responses.
normalAction :: ActionOptions wm
normalAction = ActionOptions False False

-- | Do something as long as the action isn't silent.
unlessSilent ::
  Applicative m
  => Args wm v -- ^ Action arguments.
  -> m () -- ^ Computation to do.
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

instance ArgsHaveMainObject a a where
  argsMainObject = castOptic $ iso id id

instance (ArgsHaveMainObject vars o) => ArgsHaveMainObject (Args wm vars) o where
  argsMainObject = #variables % argsMainObject

instance (ArgsHaveMainObject vars o) => ArgsMightHaveMainObject (Args wm vars) o where
  argsMainObjectMaybe = #variables % argsMainObjectMaybe

instance {-# OVERLAPS #-} (ArgsHaveMainObject vars o) => ArgsMightHaveMainObject vars o where
  argsMainObjectMaybe = castOptic argsMainObject

getActorLocation ::
  NoMissingObjects wm es
  => Args wm v
  -> Eff es (Room wm)
getActorLocation args = getLocation $ source args
