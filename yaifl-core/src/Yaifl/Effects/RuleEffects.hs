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

newtype ActivityCollector wm = ActivityCollector { activityCollection :: WMActivities wm }
newtype ResponseCollector wm = ResponseCollector { responseCollection :: WMResponses wm }
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
