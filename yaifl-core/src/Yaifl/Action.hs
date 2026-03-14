{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Yaifl.Action
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Actions represent commands that players type or that NPCs execute, following the Inform7 action model.
This module provides the foundation for Yaifl's interactive fiction command system.

The action system includes:

- `Action`: Core action type with comprehensive lifecycle phases (before/instead/check/carry out/report/after)
- `ActionRulebook`: Specialized rulebooks for each action phase
- `ParseArguments`: Argument parsing system equivalent to Inform7's "understand" grammar
- `ActionPhrase`: Container for different action types (regular, interpreted, out-of-world)
- Helper functions for creating and working with actions

Actions follow the Inform7 lifecycle model:
- Argument parsing and understanding
- Before rules (setup and validation)
- Instead rules (alternative behaviors)
- Check rules (precondition checking)
- Carry out rules (main execution)
- Report rules (output generation)
- After rules (cleanup and side effects)

This modular approach enables:
- Fine-grained control over action behavior
- Reusable rule components across actions
- Type-safe action definitions
- Integration with Yaifl's effect system

See also:
- `Yaifl.Rulebook` for the underlying rulebook system
- `Yaifl.Actions.Args` for argument handling
- `Yaifl.Actions.GoesWith` for action signature patterns
-}

module Yaifl.Action
  ( -- * Core Types
    Action(..)
  , ActionRulebook
  , ActionRule
  , WrappedAction(..)

    -- * Action Components
  , ActionPhrase(..)
  , InterpretAs(..)
  , OutOfWorldAction(..)

    -- * Argument Processing
  , ParseArguments(..)
  , ParseArgumentEffects
  , ParseArgumentResult(..)

    -- * Action Lifecycle
  , ActionInterrupt(..)
  , withActionInterrupt'

    -- * Action Construction
  , makeAction
  , makeActionRulebook

    -- * Action Utilities
  , actionName
  , getAllRules
  ) where

import Yaifl.Prelude hiding (Reader)

import Yaifl.WorldModel ( WorldModel )
import Yaifl.Rulebook
import Yaifl.Actions.Args
import Yaifl.Metadata
import Yaifl.Effects.ObjectQuery
import qualified Data.Text as T
import Effectful.Reader.Static
import Yaifl.Thing.Kind
import Effectful.Error.Static
import Yaifl.Actions.GoesWith
import Yaifl.Effects.RuleEffects
import Yaifl.Text.Responses
import Yaifl.Refreshable
import Yaifl.Effects.Print (printLn)

type ParseArgumentEffects wm es = (WithMetadata es, WithoutMissingObjects wm es, RuleEffects wm es)

-- | The result of attempting to parse action arguments.
-- Represents success, failure, or conversion to a different action pattern.
data ParseArgumentResult wm v =
  FailedParse Text
  | SuccessfulParse v
  | ConversionTo Text [ActionParameter wm]
  deriving stock (Show, Generic, Functor)

-- | `ParseArguments` is the equivalent of Inform7's `set rulebook variables`.
newtype ParseArguments wm ia v = ParseArguments
  { runParseArguments :: forall es. (ParseArgumentEffects wm es, Refreshable wm v) => ia -> Eff es (ParseArgumentResult wm v)
  }

-- | An 'Action' is a command that the player types, or that an NPC chooses to execute.
-- Pretty much all of it is lifted directly from the Inform concept of an action,
-- except that set action variables is not a rulebook.
data Action (wm :: WorldModel) resps (goesWith :: ActionSignature) v where
  Action ::
    { name :: Text
    , understandAs :: [Text]
    , matches :: [(Text, ActionSignature)]
    , touchableNouns :: Args wm v -> [Thing wm]
    , responses :: resps -> Response wm (Args wm v)
    , parseArguments :: ParseArguments wm (UnverifiedArgs wm goesWith) v
    , beforeRules :: ActionRulebook wm (Action wm resps goesWith v) v
    , insteadRules :: ActionRulebook wm (Action wm resps goesWith v) v
    , checkRules :: ActionRulebook wm (Action wm resps goesWith v) v
    , carryOutRules :: ActionRulebook wm (Action wm resps goesWith v) v
    , reportRules :: ActionRulebook wm (Action wm resps goesWith v) v
    , afterRules :: ActionRulebook wm (Action wm resps goesWith v) v
    } -> Action wm resps goesWith v
  deriving stock (Generic)

-- | A wrapper around an `Action` that provides type-safe constraints.
-- Ensures the action's value type is refreshable, follows the goes-with pattern,
-- and is displayable before allowing construction.
data WrappedAction (wm :: WorldModel) where
  WrappedAction ::
    (Refreshable wm v, GoesWith goesWith, Display v)
    => Action wm resp goesWith v
    -> WrappedAction wm

-- | An action that operates outside the normal game world context.
-- Used for meta-commands, debugging actions, or system-level operations
-- that don't follow the standard action lifecycle.
data OutOfWorldAction wm = OutOfWorldAction
  { name :: Text
  , runOutOfWorldAction :: forall es. RuleEffects wm es => Eff es ()
  }

-- | 'ActionRulebook's run over specific arguments; specifically, they expect
-- their arguments to be pre-verified; this allows for the passing of state.
type ActionRulebook wm ac v = Rulebook wm ((:>) (Reader ac)) (Args wm v) Bool
type ActionRule wm ac v = Rule wm ((:>) (Reader ac)) (Args wm v) Bool

-- | Controls whether action processing should continue or stop.
-- Used to interrupt the normal action lifecycle when appropriate.
data ActionInterrupt = ContinueAction | StopAction
  deriving stock (Eq, Ord, Enum, Bounded, Generic, Read, Show)

-- | A container that can hold different types of action-like operations.
-- Used to unify regular actions, interpreted actions, and out-of-world actions
-- in a single type for processing and dispatch.
data ActionPhrase (wm :: WorldModel) =
  Interpret (InterpretAs wm)
  | RegularAction (WrappedAction wm)
  | OtherAction (OutOfWorldAction wm)
  deriving stock ( Generic )

-- | If we should interpret some verb as another action (possibly which then points to another interpret as)
data InterpretAs wm = InterpretAs
  { toParseAs :: Text
  , withArgs :: [ActionParameter wm]
  }

makeFieldLabelsNoPrefix ''Action

withActionInterrupt' ::
  Eff (Error ActionInterrupt : es) (Maybe Bool)
  -> Eff es (Maybe Bool)
withActionInterrupt' f = do
  r <- runErrorNoCallStack f
  case r of
    -- TODO: investigate what the callstack adds
    Left ContinueAction -> rulePass
    Left StopAction -> return $ Just False
    Right x -> return x

-- | Get the name of an action. This is mostly here to avoid overlapping instances with label optics and duplicate fields.
actionName :: Lens' (Action wm resp goesWith v) Text
actionName = #name

makeAction ::
  Text
  -> Action wm resp goesWith v
makeAction n = Action
  { name = n
  , understandAs = [n]
  , matches = []
  , touchableNouns = const []
  , responses = \_ -> notImplementedResponse "no response"
  , parseArguments = ParseArguments $ const $ pure $ FailedParse "not parsed"
  , beforeRules = makeActionRulebook ("before " <> n <> " rulebook") []
  , insteadRules = makeActionRulebook ("instead " <> n <> " rulebook") []
  , carryOutRules = makeActionRulebook ("carry out " <> n <> " rulebook")
      [makeRule' "not implemented action rule" $ printLn "This action has not been implemented." >> rulePass]
  , afterRules = makeActionRulebook ("after " <> n <> " rulebook") []
  , checkRules = makeActionRulebook ("check " <> n <> " rulebook") []
  , reportRules = makeActionRulebook ("report " <> n <> " rulebook") []
  }

-- | Helper function to make a rulebook of an action; since there are a lot of these for each action,
-- we ignore the span to avoid clutter and thread the arguments through.
makeActionRulebook ::
  Text -- ^ the name of the rule.
  -> [Rule wm ((:>) (Reader (Action wm resps goesWith v))) (Args wm v) Bool] -- ^ the list of rules.
  -> ActionRulebook wm (Action wm resps goesWith v) v
makeActionRulebook n = Rulebook n Nothing

-- | Get a comma-separated string of all rule names from an action's rulebooks.
-- This includes rules from all action phases: before, instead, check, carry out, report, and after.
-- Primarily useful for debugging and introspection purposes.
getAllRules ::
  Action wm resp goesWith v
  -> Text
getAllRules Action{..} = T.intercalate "," . mconcat . map getRuleNames $
  [ beforeRules, insteadRules, checkRules, carryOutRules, reportRules, afterRules ]
