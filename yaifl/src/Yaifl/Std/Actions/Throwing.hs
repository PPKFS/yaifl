module Yaifl.Std.Actions.Throwing
  ( ThrowingResponses(..)
  , ThrowingAction
  , ThrowingRule
  , throwingAction
  , throwingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data ThrowingResponses wm =
  FooA

throwingResponses :: ThrowingResponses wm -> Response wm (Args wm (Thing wm))
throwingResponses = \case
  _ -> notImplementedResponse "response"

type ThrowingAction wm = Action wm (ThrowingResponses wm) 'TakesNoParameter (Thing wm)
type ThrowingRule wm = ActionRule wm (ThrowingAction wm) (Thing wm)
throwingAction :: ThrowingAction wm
throwingAction = (makeAction "throwing")
  { responses = throwingResponses
  , checkRules = makeActionRulebook "check throwing" ([] <> map notImplementedRule
    [ "can't do throwing"
    ])
  , carryOutRules = makeActionRulebook "carry out throwing" [ notImplementedRule "standard throwing"  ]
  , reportRules = makeActionRulebook "report throwing"  [ notImplementedRule "standard report throwing"  ]
  }
