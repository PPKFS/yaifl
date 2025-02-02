module Yaifl.Std.Actions.Waking
  ( WakingResponses(..)
  , WakingAction
  , WakingRule
  , wakingAction
  , wakingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data WakingResponses wm =
  FooA

wakingResponses :: WakingResponses wm -> Response wm (Args wm (Thing wm))
wakingResponses = \case
  _ -> notImplementedResponse "response"

type WakingAction wm = Action wm (WakingResponses wm) 'TakesNoParameter (Thing wm)
type WakingRule wm = ActionRule wm (WakingAction wm) (Thing wm)
wakingAction :: WakingAction wm
wakingAction = (makeAction "waking")
  { responses = wakingResponses
  , checkRules = makeActionRulebook "check waking" ([] <> map notImplementedRule
    [ "can't do waking"
    ])
  , carryOutRules = makeActionRulebook "carry out waking" [ notImplementedRule "standard waking"  ]
  , reportRules = makeActionRulebook "report waking"  [ notImplementedRule "standard report waking"  ]
  }
