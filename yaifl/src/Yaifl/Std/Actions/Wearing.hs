module Yaifl.Std.Actions.Wearing
  ( WearingResponses(..)
  , WearingAction
  , WearingRule
  , wearingAction
  , wearingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data WearingResponses wm =
  WearNotClothingA
  | WearAlreadyWornA
  | WearReportA
  | WearReportB
wearingResponses :: WearingResponses wm -> Response wm (Args wm (Thing wm))
wearingResponses = \case
  _ -> notImplementedResponse "response"

type WearingAction wm = Action wm (WearingResponses wm) 'TakesNoParameter (Thing wm)
type WearingRule wm = ActionRule wm (WearingAction wm) (Thing wm)
wearingAction :: WearingAction wm
wearingAction = (makeAction "wearing")
  { responses = wearingResponses
  , checkRules = makeActionRulebook "check wearing" ([] <> map notImplementedRule
    [ "can't wear what's not held"
    , "can't wear what's not clothing"
    , "can't wear already wearing"
    ])
  , carryOutRules = makeActionRulebook "carry out wearing" [ notImplementedRule "standard wearing"  ]
  , reportRules = makeActionRulebook "report wearing"  [ notImplementedRule "standard report wearing"  ]
  }
