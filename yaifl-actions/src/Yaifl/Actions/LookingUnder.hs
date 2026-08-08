module Yaifl.Actions.LookingUnder
  ( LookingUnderResponses(..)
  , LookingUnderAction
  , LookingUnderRule
  , lookingUnderAction
  , lookingUnderResponses
  ) where

import Yaifl.Actions.Imports
import Yaifl.Thing.Kind
import Yaifl.ActionOn

data LookingUnderResponses wm =
  LookUnderReportA
  | LookUnderCarryOutA

lookingUnderResponses :: LookingUnderResponses wm -> Response wm (Args wm (Thing wm))
lookingUnderResponses = \case
  _ -> notImplementedResponse "response"

type LookingUnderAction wm = Action wm (LookingUnderResponses wm) 'TakesThingParameter (Thing wm)
type LookingUnderRule wm = ActionRule wm (LookingUnderAction wm) (Thing wm)
lookingUnderAction :: LookingUnderAction wm
lookingUnderAction = (makeAction "lookingUnder")
  { understandAs =  ["look under"]
  , parseArguments = actionOnOneThing
  , responses = lookingUnderResponses
  , carryOutRules = makeActionRulebook "carry out lookingUnder" [ notImplementedRule "standard lookingUnder"  ]
  , reportRules = makeActionRulebook "report lookingUnder"  [ notImplementedRule "standard report lookingUnder"  ]
  }
