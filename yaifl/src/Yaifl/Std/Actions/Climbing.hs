module Yaifl.Std.Actions.Climbing
  ( ClimbingResponses(..)
  , ClimbingAction
  , ClimbingRule
  , climbingAction
  , climbingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data ClimbingResponses wm =
  FooA

climbingResponses :: ClimbingResponses wm -> Response wm (Args wm (Thing wm))
climbingResponses = \case
  _ -> notImplementedResponse "response"

type ClimbingAction wm = Action wm (ClimbingResponses wm) 'TakesNoParameter (Thing wm)
type ClimbingRule wm = ActionRule wm (ClimbingAction wm) (Thing wm)
climbingAction :: ClimbingAction wm
climbingAction = (makeAction "climbing")
  { responses = climbingResponses
  , checkRules = makeActionRulebook "check climbing" ([] <> map notImplementedRule
    [ "can't do climbing"
    ])
  , carryOutRules = makeActionRulebook "carry out climbing" [ notImplementedRule "standard climbing"  ]
  , reportRules = makeActionRulebook "report climbing"  [ notImplementedRule "standard report climbing"  ]
  }
