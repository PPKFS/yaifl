module Yaifl.Std.Actions.Touching
  ( TouchingResponses(..)
  , TouchingAction
  , TouchingRule
  , touchingAction
  , touchingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data TouchingResponses wm =
  FooA

touchingResponses :: TouchingResponses wm -> Response wm (Args wm (Thing wm))
touchingResponses = \case
  _ -> notImplementedResponse "response"

type TouchingAction wm = Action wm (TouchingResponses wm) 'TakesNoParameter (Thing wm)
type TouchingRule wm = ActionRule wm (TouchingAction wm) (Thing wm)
touchingAction :: TouchingAction wm
touchingAction = (makeAction "touching")
  { responses = touchingResponses
  , checkRules = makeActionRulebook "check touching" ([] <> map notImplementedRule
    [ "can't do touching"
    ])
  , carryOutRules = makeActionRulebook "carry out touching" [ notImplementedRule "standard touching"  ]
  , reportRules = makeActionRulebook "report touching"  [ notImplementedRule "standard report touching"  ]
  }
