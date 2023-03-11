{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Core.Rules.Rule
  ( Rule(..)
  , RuleCondition(..)
  , RuleLimitedEffect(..)
  --, runRuleLimitedEffect
  , parseAction
  , notImplementedRule
  , makeRule
  , makeRule'
  , rulePass
  , ruleCondition
  , ruleCondition'
  , sayText
  ) where

import Solitude

import Breadcrumbs ( ignoreSpan, addAnnotation )
import Effectful.Error.Static ( Error, throwError )
import Text.Interpolation.Nyan ( rmode', int )
import Yaifl.Core.Rules.Args ( Refreshable )
import Yaifl.Core.Rules.RuleEffects

newtype RuleLimitedEffect wm es a = RuleLimitedEffect (Eff (es : ConcreteRuleStack wm) a)

newtype Precondition wm v = Precondition
  { checkPrecondition :: forall es. RuleEffects wm es => v -> Eff es Bool }

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule wm v r = Rule
  { name :: Text
  --, preconditions :: Maybe [Precondition wm v]
  , runRule :: forall es. (RuleEffects wm es, Error RuleCondition :> es, Refreshable wm v) => v -> Eff es (Maybe v, Maybe r)
  }

-- | A helper for rules which are not implemented and therefore blank.
notImplementedRule ::
  Text
  -> Rule wm v r
notImplementedRule n = makeRule' n (do
  ignoreSpan -- this will discard the rule span
  addAnnotation [int|t| Rule #{n} needs implementing|]
  return Nothing)

-- | Make a rule that does not modify the action arguments.
makeRule ::
  Text -- ^ Rule name.
  -> (forall es. (RuleEffects wm es, Error RuleCondition :> es, Refreshable wm v) => v -> Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm v r
makeRule n f = Rule n (fmap (Nothing, ) . f)

-- | Make a rule that has no arguments. this is more convenient to avoid \() ->...
makeRule' ::
  Text -- ^ Rule name.
  -> (forall es. (RuleEffects wm es, Error RuleCondition :> es) => Eff es (Maybe r)) -- ^ Rule function.
  -> Rule wm v r
makeRule' n f = makeRule n (const f)

-- | Remove any unwanted return values from a `Rule`.
rulePass ::
  Monad m
  => m (Maybe a)
rulePass = return Nothing

makeFieldLabelsNoPrefix ''Rule

ruleCondition' ::
  Error RuleCondition :> es
  => Eff es Bool
  -> Eff es ()
ruleCondition' f = ifM f pass $ throwError RuleCondition

ruleCondition ::
  Error RuleCondition :> es
  => Eff es (Maybe a)
  -> Eff es a
ruleCondition f = do
  mbC <- f
  case mbC of
    Nothing -> throwError RuleCondition
    Just r -> return r
