module Yaifl.Std.Actions.TakingOff
  ( TakingOffResponses(..)
  , TakingOffAction
  , TakingOffRule
  , takingOffAction
  , takingOffResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data TakingOffResponses wm =
  FooA

takingOffResponses :: TakingOffResponses wm -> Response wm (Args wm (Thing wm))
takingOffResponses = \case
  _ -> notImplementedResponse "response"

type TakingOffAction wm = Action wm (TakingOffResponses wm) 'TakesNoParameter (Thing wm)
type TakingOffRule wm = ActionRule wm (TakingOffAction wm) (Thing wm)
takingOffAction :: TakingOffAction wm
takingOffAction = (makeAction "takingOff")
  { responses = takingOffResponses
  , checkRules = makeActionRulebook "check takingOff" ([] <> map notImplementedRule
    [ "can't do takingOff"
    ])
  , carryOutRules = makeActionRulebook "carry out takingOff" [ notImplementedRule "standard takingOff"  ]
  , reportRules = makeActionRulebook "report takingOff"  [ notImplementedRule "standard report takingOff"  ]
  }
