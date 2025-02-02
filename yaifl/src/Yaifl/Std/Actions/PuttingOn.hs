module Yaifl.Std.Actions.PuttingOn
  ( PuttingOnResponses(..)
  , PuttingOnAction
  , PuttingOnRule
  , puttingOnAction
  , puttingOnResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data PuttingOnResponses wm =
  PuttingOnItselfA
  | PuttingOnSupporterA
  | PuttingOnClothesA
  | PuttingOnExceedsCapacityA
  | PuttingOnConciseReportA
  | PuttingOnStandardReportA


puttingOnResponses :: PuttingOnResponses wm -> Response wm (Args wm (Thing wm))
puttingOnResponses = \case
  _ -> notImplementedResponse "response"

type PuttingOnAction wm = Action wm (PuttingOnResponses wm) 'TakesNoParameter (Thing wm)
type PuttingOnRule wm = ActionRule wm (PuttingOnAction wm) (Thing wm)
puttingOnAction :: PuttingOnAction wm
puttingOnAction = (makeAction "putting on")
  { responses = puttingOnResponses
  , checkRules = makeActionRulebook "check putting on" ([] <> map notImplementedRule
    [ "convert put to drop where possible"
    , "can't put what's not held"
    , "can't put something on itself"
    , "can't put on what's not a supporter"
    , "can't put clothes being worn"
    , "can't put if exceeds carrying capacity"
    ])
  , carryOutRules = makeActionRulebook "carry out putting on" [ notImplementedRule "standard putting on"  ]
  , reportRules = makeActionRulebook "report putting on"  [ notImplementedRule "concise report putting", notImplementedRule "standard report putting" ]
  }
