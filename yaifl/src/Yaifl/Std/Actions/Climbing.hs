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

type ClimbingAction wm = Action wm (ClimbingResponses wm) 'TakesThingParameter (Thing wm)
type ClimbingRule wm = ActionRule wm (ClimbingAction wm) (Thing wm)
climbingAction :: ClimbingAction wm
climbingAction = (makeAction "climbing")
  { name = "climbing"
  , understandAs = ["climb", "clamber"]
  , responses = climbingResponses
  , parseArguments = actionOnOneThing
  , checkRules = makeActionRulebook "check climbing" [notImplementedRule "can't do climbing"]
  , carryOutRules = makeActionRulebook "carry out climbing" [ notImplementedRule "standard climbing"  ]
  , reportRules = makeActionRulebook "report climbing"  [ notImplementedRule "standard report climbing"  ]
  }
