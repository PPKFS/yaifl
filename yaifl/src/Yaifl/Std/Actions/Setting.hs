module Yaifl.Std.Actions.Setting
  ( SettingResponses(..)
  , SettingAction
  , SettingRule
  , settingAction
  , settingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data SettingResponses wm =
  FooA

settingResponses :: SettingResponses wm -> Response wm (Args wm (Thing wm))
settingResponses = \case
  _ -> notImplementedResponse "response"

type SettingAction wm = Action wm (SettingResponses wm) 'TakesNoParameter (Thing wm)
type SettingRule wm = ActionRule wm (SettingAction wm) (Thing wm)
settingAction :: SettingAction wm
settingAction = (makeAction "setting")
  { responses = settingResponses
  , checkRules = makeActionRulebook "check setting" ([] <> map notImplementedRule
    [ "can't do setting"
    ])
  , carryOutRules = makeActionRulebook "carry out setting" [ notImplementedRule "standard setting"  ]
  , reportRules = makeActionRulebook "report setting"  [ notImplementedRule "standard report setting"  ]
  }
