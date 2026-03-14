
{-|
Module      : Yaifl.Rulebook
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Rulebooks provide a modular, ordered collection of named processing steps (rules) that can be dynamically composed and executed.
Each rule is a named function that may have preconditions determining when it applies to a specific invocation.

This module defines the fundamental rulebook system:

- `Rulebook`: An ordered collection of rules with a default outcome
- `Rule`: Individual named processing steps with optional preconditions
- `Precondition`: Conditions that must be satisfied for a rule to execute
- Helper functions for creating, modifying, and querying rulebooks

The rulebook execution model:
- Iterates through rules in order
- Checks preconditions for each rule
- Executes applicable rules until one produces a result or halts execution
- Returns default value if no rules produce a result

Rules act as modular building blocks that can be:
- Added, removed, or reordered by name
- Dynamically composed at runtime
- Shared across different rulebook configurations

See also:
- `Yaifl.Effects.RuleEffects` for the effect system used by rules
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

-- | A precondition that must be satisfied for a rule to execute.
-- Preconditions have a name (for debugging) and a check function that determines applicability.
data Precondition wm v = Precondition
  { preconditionName :: forall es. RuleEffects wm es => Eff es Text
  , checkPrecondition :: forall es. RuleEffects wm es => v -> Eff es Bool
  }

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule wm (x :: [Effect] -> Constraint) v r = Rule
  { name :: Text
  , preconditions :: [Precondition wm v]
  , runRule :: forall es. (RuleEffects wm es, Refreshable wm v, x es, SayableValue (WMText wm) wm) => v -> Eff es (Maybe v, Maybe r)
  }

-- | A constraint synonym that imposes no additional constraints.
-- Used as a type parameter in `Rulebook` when no specific effect constraints are required.
-- This allows rulebooks to work with the basic `RuleEffects` without additional requirements.
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
data Rulebook wm x v r = Rulebook
  { name :: Text
  , defaultOutcome :: Maybe r
  , rules :: [Rule wm x v r]
  } deriving stock (Generic)

-- | Get the names of all rules in a rulebook.
-- This function is primarily intended for debugging purposes.
-- It returns a list of rule names, with blank rules marked as "blank rule".
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
