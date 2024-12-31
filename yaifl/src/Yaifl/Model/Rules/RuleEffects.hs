module Yaifl.Model.Rules.RuleEffects
  ( RuleEffects
  , ActionHandler(..)
  , parseAction
  , ActivityCollector(..)
  , ResponseCollector(..)
  , ConcreteRuleStack
  , SayableValue(..)
  ) where

import Yaifl.Prelude

import Breadcrumbs ( Breadcrumbs )
import Effectful.Error.Static ( Error )
import Effectful.TH ( makeEffect )
import Effectful.Writer.Static.Local
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Core.Metadata ( Metadata )
import Yaifl.Text.Print ( Print, printText )
import Yaifl.Core.WorldModel ( WMActivities, WMResponses )
import Yaifl.Core.Effects
import Yaifl.Core.Actions.Args
import Yaifl.Core.Actions.GoesWith
import Yaifl.Model.Input

data ActionHandler wm :: Effect where
  ParseAction :: ActionOptions wm -> [NamedActionParameter wm] -> Text -> ActionHandler wm m (Either Text Bool)

newtype ActivityCollector wm = ActivityCollector { activityCollection :: WMActivities wm }
newtype ResponseCollector wm = ResponseCollector { responseCollection :: WMResponses wm }

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

class SayableValue s wm where
  sayTell :: (Writer Text :> es, RuleEffects wm es) => s -> Eff es ()
  say :: RuleEffects wm es => s -> Eff es ()
  default say :: RuleEffects wm es => s -> Eff es ()
  say s = do
    r <- execWriter (sayTell s)
    when (r /= "") $ printText r

instance SayableValue Text wm where
  sayTell = tell

instance SayableValue String wm where
  sayTell = tell . toText

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