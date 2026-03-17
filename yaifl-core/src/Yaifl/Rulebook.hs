
{-|
Module      : Yaifl.Rulebook
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Modular, composable rule system for game logic.

This module implements Yaifl's powerful rulebook system, which provides a
flexible, declarative approach to game logic. Rulebooks are ordered collections
of named rules that can be dynamically composed and executed, forming the
foundation for:

- **Action processing**: Before/check/carry out/report rules
- **Activity execution**: Mechanical game processes
- **Game mechanics**: Customizable behavior patterns
- **AI decision making**: Rule-based NPC logic

Core Components:

- `Rulebook`: Container for ordered rules with default outcome
- `Rule`: Individual processing steps with names and preconditions
- `Precondition`: Conditions that gate rule execution
- Comprehensive API for rulebook manipulation

The Rulebook Execution Model:

Rulebooks execute according to a predictable pattern:

1. **Iteration**: Process rules in order (first to last)
2. **Precondition Check**: Evaluate each rule's preconditions
3. **Execution**: Run the first applicable rule that produces a result
4. **Fallback**: Return default value if no rules produce results
5. **Short-circuiting**: Stop execution if a rule explicitly halts

This model enables:

- **Modularity**: Rules are self-contained units of logic
- **Composability**: Rulebooks can be combined and extended
- **Dynamic behavior**: Rules can be added/removed at runtime
- **Declarative style**: Logic expressed as "what should happen" rather than "how to make it happen"
- **Override patterns**: Later rules can override earlier ones

Key Features:

- **Named rules**: Each rule has a unique identifier for management
- **Preconditions**: Fine-grained control over rule applicability
- **Default outcomes**: Fallback behavior when no rules match
- **Rule ordering**: Execution order can be dynamically adjusted
- **Rule sharing**: Common rules can be reused across rulebooks

Example Usage:

@
  -- Create a rulebook for taking objects
  takeRulebook = blankRulebook "take"
    `addRuleLast` canTakeRule
    `addRuleLast` takeFromContainerRule
    `addRuleLast` takeFromSupporterRule
    
  -- Define a rule with preconditions
  canTakeRule = makeRule "canTake" [reachable, visible] $ \vars -> do
    ensureNotFixed (target vars)
    ensureCapacity (destination vars)
    
  -- Execute the rulebook
  result <- runRulebook takeRulebook player target
@

Related Modules:

- `Yaifl.Effects.RuleEffects`: Effect system integration for rules
- `Yaifl.Action`: Action system that uses rulebooks extensively
- `Yaifl.Activity`: Activity system built on rulebooks
- `Yaifl.Core.Rules`: Additional rule utilities and patterns

The rulebook system is one of Yaifl's most powerful features, enabling
declarative, modular, and maintainable game logic that can be easily
extended and customized.
-}

module Yaifl.Rulebook
  ( -- * Core Types
    Rulebook(..)
  , Rule(..)
  , Precondition(..)

    -- * Rulebook Construction
  , blankRulebook
  , addRuleFirst
  , addRuleLast

    -- * Rule Creation
  , makeRule
  , makeRule'
  , notImplementedRule

    -- * Rule Utilities
  , rulePass
  , ruleWhen
  , ruleWhenJustM
  , ruleGuard
  , ruleGuardM
  , stopTheAction

    -- * Debug Functions
  , getRuleNames

    -- * Constraint Utilities
  , Unconstrained
  ) where


import Yaifl.Prelude
import Breadcrumbs

import Yaifl.WorldModel
import Yaifl.Effects.RuleEffects
import Yaifl.Refreshable
import Yaifl.Text.SayableValue

-- | Rule execution precondition.
--
-- Preconditions determine whether a rule should be considered for execution.
-- Each precondition has:
-- - A name (for debugging and logging)
-- - A check function that evaluates the precondition
--
-- Preconditions enable fine-grained control over when rules apply, allowing
-- for context-sensitive behavior without complex conditional logic in the
-- rule itself.
--
-- Example:
-- @
--   -- Precondition that checks if an object is reachable
--   reachablePrecondition = Precondition
--     { preconditionName = pure "reachable"
--     , checkPrecondition = \vars -> isReachable (target vars)
--     }
--   
--   -- Rule that only executes when target is reachable
--   takeRule = makeRule "take" [reachablePrecondition] $ \vars -> do
--     moveToInventory (target vars)
-- @
--
-- Multiple preconditions can be combined - all must pass for the rule to execute.
data Precondition wm v = Precondition
  { preconditionName :: forall es. RuleEffects wm es => Eff es Text
  -- ^ Name of the precondition (for debugging)
  , checkPrecondition :: forall es. RuleEffects wm es => v -> Eff es Bool
  -- ^ Function that checks if the precondition is satisfied
  }

-- | Individual processing step within a rulebook.
--
-- A `Rule` represents a single step in a rulebook's execution. Each rule has:
-- - A unique name for identification and management
-- - Zero or more preconditions that must be satisfied
-- - A main function that executes the rule logic
--
-- The rule function receives the current variables and can:
-- - Return `Nothing`: Rule doesn't produce a result (continue to next rule)
-- - Return `Just newVars`: Rule produces a result with updated variables
-- - Return `Just result`: Rule produces a final result
--
-- Rules are the fundamental building blocks of Yaifl's behavior system,
-- enabling modular, reusable, and composable game logic.
--
-- Example:
-- @
--   -- Simple rule that always succeeds
--   simpleRule = makeRule "alwaysSucceed" [] $ \vars -> do
--     logDebug "Rule executed"
--     return (Just vars, Just "success")
--   
--   -- Rule with preconditions
--   conditionalRule = makeRule "conditional" [isVisible, isReachable] $ \vars -> do
--     performAction (target vars)
--     return (Just vars, Just "done")
-- @
--
-- Rules can modify the game world, generate output, and control the
-- flow of execution through their return values.
data Rule wm (x :: [Effect] -> Constraint) v r = Rule
  { name :: Text
  -- ^ Unique identifier for the rule
  , preconditions :: [Precondition wm v]
  -- ^ Conditions that must be satisfied for this rule to execute
  , runRule :: forall es. (RuleEffects wm es, Refreshable wm v, x es, SayableValue (WMText wm) wm) => v -> Eff es (Maybe v, Maybe r)
  -- ^ Rule execution function (variables -> (updated variables, result))
  }

-- | Constraint synonym for unconstrained rules.
--
-- This typeclass serves as a marker for rules that don't require additional
-- effect constraints beyond the basic `RuleEffects`. It's used as a type
-- parameter in `Rule` when no specific effect requirements are needed.
--
-- The empty instance allows any type to satisfy the constraint, making it
-- useful for rulebooks that should work with minimal effect requirements.
--
-- Example:
-- @
--   -- Rulebook that works with basic effects
--   simpleRulebook :: Rulebook wm Unconstrained v r
-- @
class Unconstrained t
instance Unconstrained t

-- | A helper for rules which are not implemented and therefore blank.
notImplementedRule ::
  Text
  -> Rule wm x v r
notImplementedRule n = makeRule' n (do
  ignoreSpan -- this will discard the rule span
  addAnnotation $ "Rule " <> n <> " needs implementing"
  return Nothing)

-- | Make a rule that does not modify the action arguments.
makeRule ::
  Text -- ^ Rule name.
  -> [Precondition wm v]
  -> (forall es. (RuleEffects wm es, Refreshable wm v, x es) => v -> Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm x v r
makeRule n c f = Rule n c (fmap (Nothing, ) . f)

-- | Make a rule that has no arguments. this is more convenient to avoid \() ->...
makeRule' ::
  Text -- ^ Rule name.
  -> (forall es. (RuleEffects wm es, x es) => Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm x v r
makeRule' n f = makeRule n [] (const f)

-- | Remove any unwanted return values from a `Rule`.
rulePass ::
  Monad m
  => m (Maybe a)
rulePass = return Nothing

-- | Stop the current action by returning `Just False`.
-- This immediately terminates rulebook execution with a failure result.
stopTheAction ::
  Monad m
  => m (Maybe Bool)
stopTheAction = return (Just False)

ruleGuard ::
  Monad m
  => Bool
  -> m (Maybe b, Maybe r)
  -> m (Maybe b, Maybe r)
ruleGuard cond f = if cond then f else pure (Nothing, Nothing)

ruleWhen ::
  Monad m
  => Bool
  -> m (Maybe r)
  -> m (Maybe r)
ruleWhen cond f = if cond then f else rulePass

ruleGuardM ::
  Monad m
  => m Bool
  -> m (Maybe b, Maybe r)
  -> m (Maybe b, Maybe r)
ruleGuardM cond f = ifM cond f $ pure (Nothing, Nothing)

ruleWhenJustM ::
  Monad m
  => m (Maybe a)
  -> (a -> m (Maybe b, Maybe r))
  -> m (Maybe b, Maybe r)
ruleWhenJustM mb f = do
  m' <- mb
  maybe (pure (Nothing, Nothing)) f m'

makeFieldLabelsNoPrefix ''Rule

-- | A 'Rulebook' is a computation (ia -> m (Maybe r)) built out of an initialisation (ia -> Maybe v), a default `Maybe r`,
-- and component rules `[(Text, (v -> m (Maybe v, Maybe r))]`
-- | Container for ordered rules with default outcome.
--
-- A `Rulebook` represents a complete processing pipeline for a specific
-- game logic domain. It contains:
--
-- - `name`: Identifier for the rulebook (used in debugging)
-- - `defaultOutcome`: Fallback result if no rules produce output
-- - `rules`: Ordered list of rules to execute
--
-- Rulebooks are the primary mechanism for organizing game logic in Yaifl.
-- They enable declarative, modular behavior definition that can be:
--
-- - Dynamically composed at runtime
-- - Extended with additional rules
-- - Reused across different contexts
-- - Customized for specific scenarios
--
-- Example:
-- @
--   -- Create a rulebook for opening containers
--   openRulebook = Rulebook
--     { name = "open"
--     , defaultOutcome = Just "You can't open that."
--     , rules = [canOpenRule, openDoorRule, openContainerRule]
--     }
--   
--   -- Execute the rulebook
--   result <- runRulebook openRulebook player target
-- @
--
-- The rulebook system is designed to be flexible and extensible, supporting
-- complex game mechanics while maintaining clean separation of concerns.
data Rulebook wm x v r = Rulebook
  { name :: Text
  -- ^ Identifier for this rulebook
  , defaultOutcome :: Maybe r
  -- ^ Fallback result if no rules produce output
  , rules :: [Rule wm x v r]
  -- ^ Ordered list of rules to execute
  } deriving stock (Generic)

-- | Get the names of all rules in a rulebook.
--
-- This function is primarily intended for debugging purposes.
-- It returns a list of rule names, with blank rules marked as "blank rule".
--
-- Parameters:
-- - `rulebook`: The rulebook to inspect
--
-- Returns: List of rule names for debugging
--
-- Example:
-- @
--   -- Debug: print all rule names
--   ruleNames <- getRuleNames takeRulebook
--   logDebug $ "Rules: " <> show ruleNames
-- @
getRuleNames ::
  Rulebook wm x v r
  -> [Text]
getRuleNames r = map (\r' -> case r' ^. #name of
  "" -> r' ^. #name <> " blank rule"
  x -> x) (rules r)

-- | Create an empty rulebook with the given name.
-- The default outcome evaluates to `Nothing`, meaning no result is produced if no rules succeed.
-- This is useful for building rulebooks incrementally by adding rules.
blankRulebook ::
  Text
  -> Rulebook wm x v r
blankRulebook n = Rulebook n Nothing []

makeFieldLabelsNoPrefix ''Rulebook

-- | Add a rule to a rulebook last.
addRuleLast ::
  Rule wm x v r
  -> Rulebook wm x v r
  -> Rulebook wm x v r
addRuleLast r = #rules %~ (++ [r])

-- | Add a rule to a rulebook first.
addRuleFirst ::
  Rule wm x v r
  -> Rulebook wm x v r
  -> Rulebook wm x v r
addRuleFirst r = #rules %~ (r :)
