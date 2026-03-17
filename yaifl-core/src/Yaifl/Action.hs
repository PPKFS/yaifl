{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Yaifl.Action
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Interactive fiction action system following the Inform7 model.

This module implements Yaifl's comprehensive action system, which enables
players and NPCs to interact with the game world through commands. The system
is heavily inspired by Inform7 but adapted to Haskell's type system.

Core Components:

- `Action`: Core action type with lifecycle phases
- `ActionRulebook`: Phase-specific rulebooks
- `ParseArguments`: Type-safe argument parsing
- `ActionPhrase`: Container for action variants
- `WrappedAction`: Runtime action representation

The Inform7 Action Lifecycle:

Yaifl's actions follow Inform7's proven lifecycle model:

1. **Argument Parsing**: Convert player input to typed action variables
2. **Before Rules**: Setup, validation, and pre-execution logic
3. **Instead Rules**: Alternative behaviors that replace normal execution
4. **Check Rules**: Precondition checking and validation
5. **Carry Out Rules**: Main execution logic
6. **Report Rules**: Output generation and player feedback
7. **After Rules**: Cleanup and side effects

This phased approach provides:

- **Modularity**: Each phase can be customized independently
- **Reusability**: Rules can be shared across different actions
- **Flexibility**: Fine-grained control over action behavior
- **Extensibility**: Easy to add new actions and modify existing ones

Key Features:

- **Type Safety**: Haskell's type system ensures actions are well-formed
- **Effect Integration**: Seamless integration with Yaifl's effect system
- **Rule-Based**: Declarative rule system for behavior customization
- **Lifecycle Hooks**: Comprehensive control over action execution
- **Error Handling**: Phase-appropriate error reporting

Example Usage:

@
  -- Define a simple action
  takeAction :: Action wm TakeVariables
  takeAction = makeAction "take" [
    -- Before rules: setup
    beforeRule $ \_ -> logDebug "Preparing to take object",
    
    -- Check rules: validation
    checkRule $ \vars -> do
      ensureReachable (target vars)
      ensureVisible (target vars)
    
    -- Carry out rules: main logic
    carryOutRule $ \vars -> do
      moveObject (target vars) inventory
      
    -- Report rules: feedback
    reportRule $ \vars -> say "You take the {target vars}."
    ]

  -- Execute the action
  result <- runAction takeAction player targetObject
@

Related Modules:

- `Yaifl.Rulebook`: Underlying rulebook system
- `Yaifl.Actions.Args`: Argument handling and payload types
- `Yaifl.Actions.GoesWith`: Action parameter patterns and parsing
- `Yaifl.Effects.ActionHandler`: Action execution effects
- `Yaifl.Text.Responses`: Response generation system

This module forms the heart of Yaifl's interactive fiction capabilities,
enabling rich, flexible, and type-safe player interactions.
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
-- | Core action definition with full lifecycle support.
--
-- This data type represents a complete interactive fiction action with all
-- necessary components for parsing, validation, execution, and reporting.
--
-- The `Action` type is parameterized by:
-- - `wm`: World model type
-- - `resps`: Response collection type
-- - `goesWith`: Action signature (parameter pattern)
-- - `v`: Variables type (action-specific parameters)
--
-- Fields:
-- - `name`: Human-readable action name (e.g., "take", "open")
-- - `understandAs`: Alternative verbs that trigger this action
-- - `matches`: Parameter patterns for parser matching
-- - `touchableNouns`: Objects that can be referenced by touch/proximity
-- - `responses`: Response generation function
-- - `parseArguments`: Argument parsing logic
-- - `beforeRules`: Pre-execution setup and validation
-- - `insteadRules`: Alternative behaviors that replace normal execution
-- - `checkRules`: Precondition checking
-- - `carryOutRules`: Main execution logic
-- - `reportRules`: Output generation
-- - `afterRules`: Post-execution cleanup
--
-- Example:
-- @
--   takeAction = Action
--     { name = "take"
--     , understandAs = ["grab", "pick up", "get"]
--     , matches = [("take", TakesThingParameter)]
--     , parseArguments = takeParser
--     , beforeRules = takeBeforeRules
--     , checkRules = takeCheckRules
--     , carryOutRules = takeCarryOutRules
--     , reportRules = takeReportRules
--     , afterRules = takeAfterRules
--     }
-- @
--
-- This comprehensive structure enables the full Inform7 action lifecycle while
-- maintaining type safety and modularity.
data Action (wm :: WorldModel) resps (goesWith :: ActionSignature) v where
  Action ::
    { name :: Text
    -- ^ Human-readable action name
    , understandAs :: [Text]
    -- ^ Alternative verbs that trigger this action
    , matches :: [(Text, ActionSignature)]
    -- ^ Parameter patterns for parser matching
    , touchableNouns :: Args wm v -> [Thing wm]
    -- ^ Objects referenceable by touch/proximity
    , responses :: resps -> Response wm (Args wm v)
    -- ^ Response generation function
    , parseArguments :: ParseArguments wm (UnverifiedArgs wm goesWith) v
    -- ^ Argument parsing logic
    , beforeRules :: ActionRulebook wm (Action wm resps goesWith v) v
    -- ^ Pre-execution setup and validation
    , insteadRules :: ActionRulebook wm (Action wm resps goesWith v) v
    -- ^ Alternative behaviors
    , checkRules :: ActionRulebook wm (Action wm resps goesWith v) v
    -- ^ Precondition checking
    , carryOutRules :: ActionRulebook wm (Action wm resps goesWith v) v
    -- ^ Main execution logic
    , reportRules :: ActionRulebook wm (Action wm resps goesWith v) v
    -- ^ Output generation
    , afterRules :: ActionRulebook wm (Action wm resps goesWith v) v
    -- ^ Post-execution cleanup
    } -> Action wm resps goesWith v
  deriving stock (Generic)

-- | Type-safe wrapper for `Action` values.
--
-- This GADT ensures that actions meet necessary constraints before they can be
-- constructed and used. It serves as a type-safe container that guarantees:
--
-- - The action's variables type is `Refreshable`
-- - The action follows the `goesWith` parameter pattern
-- - The action can be displayed (has `Display` instance)
--
-- The `WrappedAction` type is used throughout Yaifl to ensure that only
-- well-formed actions can be executed, preventing runtime errors and
-- maintaining type safety.
--
-- Example:
-- @
--   -- Create a wrapped action (type-safe)
--   wrappedTake <- makeAction "take" takeRules
--   
--   -- Use in action execution
--   result <- runWrappedAction wrappedTake player target
-- @
--
-- This wrapper pattern is common in Yaifl for complex types that have
-- multiple constraints, providing compile-time verification of properties
-- that would otherwise require runtime checks.
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
