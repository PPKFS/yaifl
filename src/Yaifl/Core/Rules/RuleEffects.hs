{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Yaifl.Core.Rules.RuleEffects where

import Solitude

import Breadcrumbs ( Breadcrumbs )
import Data.Text.Display
import Effectful.Error.Static ( Error )
import Effectful.TH ( makeEffect )
import Effectful.Writer.Static.Local
import Yaifl.Core.AdaptiveNarrative
import Yaifl.Core.Metadata ( Metadata )
import Yaifl.Core.Object ( Thing )
import Yaifl.Core.Objects.Query
import Yaifl.Core.Print ( Print, printText, printLn )
import Yaifl.Core.WorldModel ( WMActivities, WMResponses, WMSayable )
import qualified Data.Text as T

data ActionHandler wm :: Effect where
  ParseAction :: ActionOptions wm -> Text -> ActionHandler wm m (Either Text Bool)

data ActionOptions wm = ActionOptions
  { silently :: Bool
  , actor :: Maybe (Thing wm)
  }

newtype ActivityCollector wm = ActivityCollector { activityCollection :: WMActivities wm }
newtype ResponseCollector wm = ResponseCollector { responseCollection :: WMResponses wm }

makeFieldLabelsNoPrefix ''ActivityCollector
makeFieldLabelsNoPrefix ''ResponseCollector
makeEffect ''ActionHandler

type RuleEffects wm es = (
  State Metadata :> es
  , State (ActivityCollector wm) :> es
  , State (ResponseCollector wm) :> es
  , State (AdaptiveNarrative wm) :> es
  , Breadcrumbs :> es
  , NoMissingObjects wm es
  , RuleConstraints wm
  , Print :> es
  , ActionHandler wm :> es
  , ObjectTraverse wm :> es
  )

type RuleConstraints wm =
  ( Display (WMSayable wm)
  , SayableValue (WMSayable wm) wm
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
  sayTell :: (Writer Text :> es, RuleEffects wm es) => s -> Eff es ()
  say :: RuleEffects wm es => s -> Eff es ()
  default say :: RuleEffects wm es => s -> Eff es ()
  say s = execWriter (sayTell s) >>= printText

instance SayableValue Text wm where
  sayTell = tell

instance SayableValue String wm where
  sayTell = tell . toText

sayText ::
  SayableValue s wm
  => RuleEffects wm es
  => s
  -> Eff es Text
sayText = execWriter . sayTell

sayLn ::
  SayableValue s wm
  => RuleEffects wm es
  => s
  -> Eff es ()
sayLn s = do
  t <- sayText s
  when (display t /= T.empty)
    (printLn $ display t)