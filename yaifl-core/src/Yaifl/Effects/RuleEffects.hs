{-|
Module      : Yaifl.Effects.RuleEffects
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

The RuleEffects system defines the core effect stack used throughout Yaifl for rule processing,
providing access to all necessary game state and operations in a single constraint.

This module defines:

- `RuleEffects`: Core effect constraint bundling all required effects
- Collector types for managing activities, responses, and actions
- `ConcreteRuleStack`: Complete effect stack for rule execution

The RuleEffects constraint includes:
- Game metadata and state management
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
  , ConcreteRuleStack
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
-- Without this newtype, `State (WMActivities wm)` could be ambiguous depending on the
-- WorldModel instantiation, as multiple components might have the same underlying type.
newtype ActivityCollector wm = ActivityCollector { activityCollection :: WMActivities wm }

-- | Wrapper around `WMResponses wm` to avoid ambiguity in state operations.
-- Without this newtype, `State (WMResponses wm)` could be ambiguous depending on the
-- WorldModel instantiation, as multiple components might have the same underlying type.
newtype ResponseCollector wm = ResponseCollector { responseCollection :: WMResponses wm }

-- | Wrapper around `WMActions wm` to avoid ambiguity in state operations.
-- Without this newtype, `State (WMActions wm)` could be ambiguous depending on the
-- WorldModel instantiation, as multiple components might have the same underlying type.
newtype ActionCollector wm = ActionCollector { activityCollection :: WMActions wm }

makeFieldLabelsNoPrefix ''ActivityCollector
makeFieldLabelsNoPrefix ''ResponseCollector

type RuleEffects wm es = (
  State Metadata :> es
  , Input :> es
  , State (ActivityCollector wm) :> es
  , State (ResponseCollector wm) :> es
  , State (AdaptiveNarrative wm) :> es
  , Breadcrumbs :> es
  , WithoutMissingObjects wm es
  , Print :> es
  , ActionHandler wm :> es
  , ObjectQuery wm :> es
  )

type ConcreteRuleStack wm = '[
  ActionHandler wm
  , Input
  , State (AdaptiveNarrative wm)
  , State (ResponseCollector wm)
  , State (ActivityCollector wm)
  , ObjectQuery wm
  , State Metadata
  , Print
  , Breadcrumbs
  , Error MissingObject
  ]
