module Yaifl.Core.Rules.RuleEffects
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

import Yaifl.Core.Actions.Args
import Yaifl.Core.Effects
import Yaifl.Core.WorldModel
import Yaifl.Core.Metadata
import Effectful.Error.Static ( Error )
import Effectful.TH ( makeEffect )
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.Print ( Print )
import Yaifl.Core.Actions.GoesWith


data ActionHandler wm :: Effect where
  ParseAction :: ActionOptions wm -> [NamedActionParameter wm] -> Text -> ActionHandler wm m (Either Text Bool)

newtype ActivityCollector wm = ActivityCollector { activityCollection :: WMActivities wm }
newtype ResponseCollector wm = ResponseCollector { responseCollection :: WMResponses wm }
newtype ActionCollector wm = ActionCollector { activityCollection :: WMActions wm }

makeFieldLabelsNoPrefix ''ActivityCollector
makeFieldLabelsNoPrefix ''ResponseCollector
makeEffect ''ActionHandler

type RuleEffects wm es = (
  State Metadata :> es
  , Input :> es
  , State (ActivityCollector wm) :> es
  , State (ResponseCollector wm) :> es
  , State (AdaptiveNarrative wm) :> es
  , Breadcrumbs :> es
  , NoMissingObjects wm es
  , Print :> es
  , ActionHandler wm :> es
  , ObjectTraverse wm :> es
  )

type ConcreteRuleStack wm = '[
  ActionHandler wm
  , Input
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
