module Yaifl.Std.Actions.Swinging
  ( SwingingResponses(..)
  , SwingingAction
  , SwingingRule
  , swingingAction
  , swingingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data SwingingResponses wm =
  FooA

swingingResponses :: SwingingResponses wm -> Response wm (Args wm (Thing wm))
swingingResponses = \case
  _ -> notImplementedResponse "response"

type SwingingAction wm = Action wm (SwingingResponses wm) 'TakesNoParameter (Thing wm)
type SwingingRule wm = ActionRule wm (SwingingAction wm) (Thing wm)
swingingAction :: SwingingAction wm
swingingAction = (makeAction "swinging")
  { responses = swingingResponses
  , checkRules = makeActionRulebook "check swinging" ([] <> map notImplementedRule
    [ "can't do swinging"
    ])
  , carryOutRules = makeActionRulebook "carry out swinging" [ notImplementedRule "standard swinging"  ]
  , reportRules = makeActionRulebook "report swinging"  [ notImplementedRule "standard report swinging"  ]
  }
