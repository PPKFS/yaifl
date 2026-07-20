
{-|
Module      : Yaifl.Rulebook
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Rulebooks provide modular, ordered collections of named processing steps (rules)
that can be dynamically composed and executed. Rules may have preconditions
determining when they apply.

Core components:
- `Rulebook`: Ordered collection of rules with default outcome
- `Rule`: Named processing steps with optional preconditions
- `Precondition`: Conditions for rule applicability

Execution model: iterate through rules, check preconditions, execute applicable rules
until one produces a result or execution halts.

Rules act as modular building blocks that can be:
- Added, removed, or reordered by name
- Dynamically composed at runtime
- Shared across different rulebook configurations

Key Features:

- **Named rules**: Each rule has a unique identifier for management
- **Preconditions**: Fine-grained control over rule applicability
- **Default outcomes**: Fallback behavior when no rules match
- **Rule ordering**: Execution order can be dynamically adjusted
- **Rule sharing**: Common rules can be reused across rulebooks
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
-- Determines whether a rule should execute. Each precondition has:
-- - A name (for debugging and logging)
-- - A check function that evaluates the precondition
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

-- | Conditionally execute a rule based on a boolean condition.
-- If the condition is false, returns `(Nothing, Nothing)` (rule pass).
ruleGuard ::
  Monad m
  => Bool
  -> m (Maybe b, Maybe r)
  -> m (Maybe b, Maybe r)
ruleGuard cond f = if cond then f else pure (Nothing, Nothing)

-- | Conditionally execute a rule based on a boolean condition.
-- If the condition is false, returns `Nothing` (rule pass).
ruleWhen ::
  Monad m
  => Bool
  -> m (Maybe r)
  -> m (Maybe r)
ruleWhen cond f = if cond then f else rulePass

-- | Conditionally execute a rule based on a monadic boolean condition.
-- If the condition is false, returns `(Nothing, Nothing)` (rule pass).
ruleGuardM ::
  Monad m
  => m Bool
  -> m (Maybe b, Maybe r)
  -> m (Maybe b, Maybe r)
ruleGuardM cond f = ifM cond f $ pure (Nothing, Nothing)

-- | Execute a rule function only if the Maybe value is Just.
-- If Nothing, returns `(Nothing, Nothing)` (rule pass).
ruleWhenJustM ::
  Monad m
  => m (Maybe a)
  -> (a -> m (Maybe b, Maybe r))
  -> m (Maybe b, Maybe r)
ruleWhenJustM mb f = do
  m' <- mb
  maybe (pure (Nothing, Nothing)) f m'

makeFieldLabelsNoPrefix ''Rule

-- | Container for ordered rules with default outcome.
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
-- Primarily intended for debugging purposes.
-- Returns a list of rule names, with blank rules marked as "blank rule".
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
