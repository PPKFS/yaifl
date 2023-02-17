{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Yaifl.Core.Rulebooks.Rule
  ( RuleEffects
  , Rule(..)
  , RuleCondition(..)
  , ActionOptions(..)
  , ActionHandler(..)
  , ActivityCollector(..)
  , ResponseCollector(..)
  , Response(..)
  , RuleLimitedEffect(..)
  , ConcreteRuleStack
  , SayableValue(..)
  --, runRuleLimitedEffect
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
import Yaifl.Core.Objects.Query ( ObjectTraverse, NoMissingObjects, ObjectUpdate, ObjectLookup, MissingObject )
import Yaifl.Core.Rulebooks.Args ( Refreshable )
import Yaifl.Core.Print ( Print, printText )
import Yaifl.Core.WorldModel ( WMActivities, WMResponses, WMSayable )
import Data.Text.Display
import Yaifl.Core.AdaptiveNarrative

data ActionHandler wm :: Effect where
  ParseAction :: ActionOptions wm -> Text -> ActionHandler wm m (Either Text Bool)

data ActionOptions wm = ActionOptions
  { silently :: Bool
  , actor :: Maybe (Thing wm)
  }

makeEffect ''ActionHandler

newtype ActivityCollector wm = ActivityCollector { activityCollection :: WMActivities wm }

newtype ResponseCollector wm = ResponseCollector { responseCollection :: WMResponses wm }

makeFieldLabelsNoPrefix ''ActivityCollector
makeFieldLabelsNoPrefix ''ResponseCollector

data RuleCondition = RuleCondition

type RuleEffects wm es = (
  State Metadata :> es
  , State (ActivityCollector wm) :> es
  , State (ResponseCollector wm) :> es
  , State (AdaptiveNarrative wm) :> es
  , Breadcrumbs :> es
  , NoMissingObjects wm es
  , Display (WMSayable wm)
  , SayableValue (WMSayable wm) wm
  , Print :> es
  , ActionHandler wm :> es
  , ObjectTraverse wm :> es
  )

type ConcreteRuleStack wm = '[
  ActionHandler wm
  , State (AdaptiveNarrative wm)
  , State (ResponseCollector wm)
  , State (ActivityCollector wm)
  , ObjectTraverse wm
  , ObjectUpdate wm
  , ObjectLookup wm
  , State Metadata
  , Print
  , Breadcrumbs
  , Error MissingObject
  ]

class SayableValue s wm where
  sayText :: RuleEffects wm es => s -> Eff es Text
  say :: RuleEffects wm es => s -> Eff es ()
  default say :: RuleEffects wm es => s -> Eff es ()
  say s = sayText s >>= printText

instance SayableValue Text wm where
  sayText = pure

newtype RuleLimitedEffect wm a = RuleLimitedEffect (Eff (ConcreteRuleStack wm) a)

-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule wm v r = Rule
  { name :: Text
  , runRule :: forall es. (RuleEffects wm es, Error RuleCondition :> es, Refreshable wm v) => v -> Eff es (Maybe v, Maybe r)
  }

newtype Response wm = Response { runResponse :: forall es. (RuleEffects wm es) => Eff es () }

makeFieldLabelsNoPrefix ''Response

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
