module Yaifl.Std.Actions.SwitchingOff
  ( SwitchingOffResponses(..)
  , SwitchingOffAction
  , SwitchingOffRule
  , switchingOffAction
  , switchingOffResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data SwitchingOffResponses wm =
  SwitchOffSwitchableA
  | SwitchOffAlreadyOffA
  | SwitchOffReportA

switchingOffResponses :: SwitchingOffResponses wm -> Response wm (Args wm (Thing wm))
switchingOffResponses = \case
  _ -> notImplementedResponse "response"

type SwitchingOffAction wm = Action wm (SwitchingOffResponses wm) 'TakesNoParameter (Thing wm)
type SwitchingOffRule wm = ActionRule wm (SwitchingOffAction wm) (Thing wm)
switchingOffAction :: SwitchingOffAction wm
switchingOffAction = (makeAction "SwitchingOff")
  { responses = switchingOffResponses
  , checkRules = makeActionRulebook "check SwitchingOff" ([] <> map notImplementedRule
    [ "can't switch off what's already off"
    , "can't switch off unless switchable"
    ])
  , carryOutRules = makeActionRulebook "carry out switching off" [ notImplementedRule "standard SwitchingOff"  ]
  , reportRules = makeActionRulebook "report SwitchingOff"  [ notImplementedRule "standard report SwitchingOff"  ]
  }
