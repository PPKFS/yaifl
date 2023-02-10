{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Core.Rulebooks.Rule
  ( RuleEffects
  , Rule(..)
  , RuleCondition(..)
  , ActionOptions(..)
  , ActionHandler(..)
  , parseAction
  , notImplementedRule
  , makeRule
  , makeRule'
  , rulePass
  , ruleCondition
  , ruleCondition'
  ) where

import Solitude

import Breadcrumbs ( Breadcrumbs, ignoreSpan, addAnnotation )
import Effectful.Error.Static ( Error, throwError )
import Effectful.TH ( makeEffect )
import Text.Interpolation.Nyan ( rmode', int )

import Yaifl.Core.Metadata ( Metadata )
import Yaifl.Core.Object ( Thing )
import Yaifl.Core.Objects.Query ( ObjectTraverse, NoMissingObjects )
import Yaifl.Core.Rulebooks.Args ( Refreshable )
import Yaifl.Core.Say ( Saying )
import Yaifl.Core.WorldModel ( WMActivities )

data ActionHandler wm :: Effect where
  ParseAction :: ActionOptions wm -> Text -> ActionHandler wm m (Either Text Bool)

data ActionOptions wm = ActionOptions
  { silently :: Bool
  , actor :: Maybe (Thing wm)
  }

makeEffect ''ActionHandler

data RuleCondition = RuleCondition

type RuleEffects wm es = (
  State Metadata :> es
  , State (WMActivities wm) :> es
  , Breadcrumbs :> es
  , NoMissingObjects wm es
  , Saying :> es
  , ActionHandler wm :> es
  , ObjectTraverse wm :> es
  )

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule wm v r = Rule
  { name :: Text
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

-- | Make a rule that does has no arguments. this is more convenient to avoid \() ->...
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
