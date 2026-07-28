{-|
Module      : Yaifl.Effects.RuleEffects
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

The RuleEffects system defines the core effect stack used throughout Yaifl for rule processing,
providing access to all necessary game state and operations in a single constraint.

The RuleEffects constraint includes:
- Game Metadata wm and state management
- Input/output operations
- Activity and response management
- Action handling
- Object querying
- Error handling
- Debugging and tracing
-}

module Yaifl.Effects.RuleEffects
  ( RuleEffects
  , ActionHandler(..)
  , parseAction
  , ActivityCollector(..)
  , ResponseCollector(..)
  , ActionCollector(..)
  , ValueCollector(..)
  , ConcreteRuleStack

  , getValue
  , setValue
  , modifyValue
  ) where

import Yaifl.Prelude
import Breadcrumbs

import Yaifl.Effects.ObjectQuery
import Yaifl.WorldModel
import Yaifl.Metadata
import Effectful.Error.Static ( Error )
import Yaifl.Effects.Input
import Yaifl.Effects.Print
import Yaifl.Effects.ActionHandler
import Yaifl.Text.AdaptiveNarrative (AdaptiveNarrative)

-- | Wrapper around `WMActivities wm` to avoid ambiguity in state operations.
-- Without this, `State (WMActivities wm)` could be ambiguous depending on the
-- WorldModel instantiation, as multiple components might have the same underlying type.
newtype ActivityCollector wm = ActivityCollector { activityCollection :: WMActivities wm }

-- | Wrapper around `WMResponses wm` to avoid ambiguity in state operations.
-- Without this, `State (WMResponses wm)` could be ambiguous depending on the
-- WorldModel instantiation, as multiple components might have the same underlying type.
newtype ResponseCollector wm = ResponseCollector { responseCollection :: WMResponses wm }

-- | Wrapper around `WMActions wm` to avoid ambiguity in state operations.
-- Without this newtype, `State (WMActions wm)` could be ambiguous depending on the
-- WorldModel instantiation, as multiple components might have the same underlying type.
newtype ActionCollector wm = ActionCollector { actionCollection :: WMActions wm }

newtype ValueCollector wm = ValueCollector { valueCollection :: WMValues wm }
  deriving stock (Generic)

makeFieldLabelsNoPrefix ''ActivityCollector
makeFieldLabelsNoPrefix ''ResponseCollector

-- |  This type synonym bundles all the effects required for rule execution.
type RuleEffects wm es = (
  State (Metadata wm) :> es
  , Input :> es
  , State (ActivityCollector wm) :> es
  , State (ResponseCollector wm) :> es
  , State (AdaptiveNarrative wm) :> es
  , State (ValueCollector wm) :> es
  , Breadcrumbs :> es
  , WithoutMissingObjects wm es
  , Print :> es
  , ActionHandler wm :> es
  , ObjectQuery wm :> es
  )

-- | Concrete effect stack implementation for rule execution.
--
-- This type provides a specific ordering of effects that satisfies the
-- 'RuleEffects' constraint.
type ConcreteRuleStack wm = '[
  ActionHandler wm
  , Input
  , State (ValueCollector wm)
  , State (AdaptiveNarrative wm)
  , State (ResponseCollector wm)
  , State (ActivityCollector wm)
  , ObjectQuery wm
  , State (Metadata wm)
  , Print
  , Breadcrumbs
  , Error MissingObject
  ]

getValue ::
  State (ValueCollector wm) :> es
  => Lens' (WMValues wm) a
  -> Eff es a
getValue l = use $ #valueCollection % l

modifyValue ::
  State (ValueCollector wm) :> es
  => Lens' (WMValues wm) a
  -> (a -> a)
  -> Eff es ()
modifyValue l f = #valueCollection % l %= f

setValue ::
  State (ValueCollector wm) :> es
  => Lens' (WMValues wm) a
  -> a
  -> Eff es ()
setValue l v = #valueCollection % l .= v