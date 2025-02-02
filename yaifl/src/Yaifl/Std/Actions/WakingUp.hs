module Yaifl.Std.Actions.WakingUp
  ( WakingUpResponses(..)
  , WakingUpAction
  , WakingUpRule
  , wakingUpAction
  , wakingUpResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data WakingUpResponses wm =
  FooA

wakingUpResponses :: WakingUpResponses wm -> Response wm (Args wm (Thing wm))
wakingUpResponses = \case
  _ -> notImplementedResponse "response"

type WakingUpAction wm = Action wm (WakingUpResponses wm) 'TakesNoParameter (Thing wm)
type WakingUpRule wm = ActionRule wm (WakingUpAction wm) (Thing wm)
wakingUpAction :: WakingUpAction wm
wakingUpAction = (makeAction "wakingUp")
  { responses = wakingUpResponses
  , checkRules = makeActionRulebook "check wakingUp" ([] <> map notImplementedRule
    [ "can't do wakingUp"
    ])
  , carryOutRules = makeActionRulebook "carry out wakingUp" [ notImplementedRule "standard wakingUp"  ]
  , reportRules = makeActionRulebook "report wakingUp"  [ notImplementedRule "standard report wakingUp"  ]
  }
