module Yaifl.Std.Actions.Dropping
  ( DroppingResponses(..)
  , DroppingAction
  , DroppingRule
  , droppingAction
  , droppingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data DroppingResponses wm =
  DropYourselfA
  | DropBodyPartsA
  | DropAlreadyDroppedA
  | DropNotHeldA
  | DropClothesWornA
  | DropExceedsCapacityA
  | DropExceedsCapacityB
  | DropReportA
  | DropReportB

droppingResponses :: DroppingResponses wm -> Response wm (Args wm (Thing wm))
droppingResponses = \case
  _ -> notImplementedResponse "response"

type DroppingAction wm = Action wm (DroppingResponses wm) 'TakesNoParameter (Thing wm)
type DroppingRule wm = ActionRule wm (DroppingAction wm) (Thing wm)
droppingAction :: DroppingAction wm
droppingAction = (makeAction "dropping")
  { responses = droppingResponses
  , checkRules = makeActionRulebook "check dropping" ([] <> map notImplementedRule
    [ "can't drop yourself"
    , "can't drop body parts"
    , "can't drop what's already dropped"
    , "can't drop what's not held"
    , "can't drop clothes being worn"
    , "can't drop if this exceeds carrying capacity"
    ])
  , carryOutRules = makeActionRulebook "carry out dropping" [ notImplementedRule "standard dropping"  ]
  , reportRules = makeActionRulebook "report dropping"  [ notImplementedRule "standard report dropping"  ]
  }
