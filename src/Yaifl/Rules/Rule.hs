{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Rules.Rule
  ( Rule(..)
  , Precondition(..)
  , RuleLimitedEffect(..)
  , forPlayer'
  , forPlayer
  , parseAction
  , notImplementedRule
  , makeRule
  , makeRule'
  , rulePass
  , ruleWhenJustM
  , ruleGuard
  , ruleGuardM
  , forThing
  ) where

import Solitude

import Breadcrumbs ( ignoreSpan, addAnnotation )
import Yaifl.Rules.Args ( Refreshable, Args, getPlayer )
import Yaifl.Rules.RuleEffects
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.Effects

newtype RuleLimitedEffect wm es a = RuleLimitedEffect (RuleConstraints wm => Eff (es : ConcreteRuleStack wm) a)

newtype Precondition wm v = Precondition
  { checkPrecondition :: forall es. RuleEffects wm es => v -> Eff es Bool }

forPlayer :: Precondition wm (Args wm v)
forPlayer = Precondition $ \v -> do
  p <- getPlayer
  pure $ p == v ^. #source

forPlayer' :: [Precondition wm (Args wm v)]
forPlayer' = [forPlayer]

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule wm v r = Rule
  { name :: Text
  , preconditions :: [Precondition wm v]
  , runRule :: forall es. (RuleEffects wm es, Refreshable wm v) => v -> Eff es (Maybe v, Maybe r)
  }

-- | A helper for rules which are not implemented and therefore blank.
notImplementedRule ::
  Text
  -> Rule wm v r
notImplementedRule n = makeRule' n (do
  ignoreSpan -- this will discard the rule span
  addAnnotation $ "Rule " <> n <> " needs implementing"
  return Nothing)

-- | Make a rule that does not modify the action arguments.
makeRule ::
  Text -- ^ Rule name.
  -> [Precondition wm v]
  -> (forall es. (RuleEffects wm es, Refreshable wm v) => v -> Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm v r
makeRule n c f = Rule n c (fmap (Nothing, ) . f)

-- | Make a rule that has no arguments. this is more convenient to avoid \() ->...
makeRule' ::
  Text -- ^ Rule name.
  -> (forall es. RuleEffects wm es => Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm v r
makeRule' n f = makeRule n [] (const f)

-- | Remove any unwanted return values from a `Rule`.
rulePass ::
  Monad m
  => m (Maybe a)
rulePass = return Nothing

ruleGuard ::
  Monad m
  => Bool
  -> m (Maybe b, Maybe r)
  -> m (Maybe b, Maybe r)
ruleGuard cond f = if cond then f else pure (Nothing, Nothing)

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

forThing ::
  NoMissingObjects wm es
  => ObjectLike wm a
  => a
  -> (Thing wm -> Eff es (Maybe b, Maybe r))
  -> Eff es (Maybe b, Maybe r)
forThing e = ruleWhenJustM (getThingMaybe e)

makeFieldLabelsNoPrefix ''Rule
