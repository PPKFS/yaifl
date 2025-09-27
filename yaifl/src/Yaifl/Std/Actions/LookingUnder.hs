module Yaifl.Std.Actions.LookingUnder
  ( LookingUnderResponses(..)
  , LookingUnderAction
  , LookingUnderRule
  , lookingUnderAction
  , lookingUnderResponses
  ) where

import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data LookingUnderResponses wm =
  LookUnderReportA
  | LookUnderCarryOutA

lookingUnderResponses :: LookingUnderResponses wm -> Response wm (Args wm (Thing wm))
lookingUnderResponses = \case
  _ -> notImplementedResponse "response"

type LookingUnderAction wm = Action wm (LookingUnderResponses wm) 'TakesNoParameter (Thing wm)
type LookingUnderRule wm = ActionRule wm (LookingUnderAction wm) (Thing wm)
lookingUnderAction :: LookingUnderAction wm
lookingUnderAction = (makeAction "lookingUnder")
  { responses = lookingUnderResponses
  , carryOutRules = makeActionRulebook "carry out lookingUnder" [ notImplementedRule "standard lookingUnder"  ]
  , reportRules = makeActionRulebook "report lookingUnder"  [ notImplementedRule "standard report lookingUnder"  ]
  }
