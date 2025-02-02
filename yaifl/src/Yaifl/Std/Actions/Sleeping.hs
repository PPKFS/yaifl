module Yaifl.Std.Actions.Sleeping
  ( SleepingResponses(..)
  , SleepingAction
  , SleepingRule
  , sleepingAction
  , sleepingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data SleepingResponses wm =
  FooA

sleepingResponses :: SleepingResponses wm -> Response wm (Args wm (Thing wm))
sleepingResponses = \case
  _ -> notImplementedResponse "response"

type SleepingAction wm = Action wm (SleepingResponses wm) 'TakesNoParameter (Thing wm)
type SleepingRule wm = ActionRule wm (SleepingAction wm) (Thing wm)
sleepingAction :: SleepingAction wm
sleepingAction = (makeAction "sleeping")
  { responses = sleepingResponses
  , checkRules = makeActionRulebook "check sleeping" ([] <> map notImplementedRule
    [ "can't do sleeping"
    ])
  , carryOutRules = makeActionRulebook "carry out sleeping" [ notImplementedRule "standard sleeping"  ]
  , reportRules = makeActionRulebook "report sleeping"  [ notImplementedRule "standard report sleeping"  ]
  }
