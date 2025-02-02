module Yaifl.Std.Actions.Turning
  ( TurningResponses(..)
  , TurningAction
  , TurningRule
  , turningAction
  , turningResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data TurningResponses wm =
  FooA

turningResponses :: TurningResponses wm -> Response wm (Args wm (Thing wm))
turningResponses = \case
  _ -> notImplementedResponse "response"

type TurningAction wm = Action wm (TurningResponses wm) 'TakesNoParameter (Thing wm)
type TurningRule wm = ActionRule wm (TurningAction wm) (Thing wm)
turningAction :: TurningAction wm
turningAction = (makeAction "turning")
  { responses = turningResponses
  , checkRules = makeActionRulebook "check turning" ([] <> map notImplementedRule
    [ "can't do turning"
    ])
  , carryOutRules = makeActionRulebook "carry out turning" [ notImplementedRule "standard turning"  ]
  , reportRules = makeActionRulebook "report turning"  [ notImplementedRule "standard report turning"  ]
  }
