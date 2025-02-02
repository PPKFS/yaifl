module Yaifl.Std.Actions.Thinking
  ( ThinkingResponses(..)
  , ThinkingAction
  , ThinkingRule
  , thinkingAction
  , thinkingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data ThinkingResponses wm =
  FooA

thinkingResponses :: ThinkingResponses wm -> Response wm (Args wm (Thing wm))
thinkingResponses = \case
  _ -> notImplementedResponse "response"

type ThinkingAction wm = Action wm (ThinkingResponses wm) 'TakesNoParameter (Thing wm)
type ThinkingRule wm = ActionRule wm (ThinkingAction wm) (Thing wm)
thinkingAction :: ThinkingAction wm
thinkingAction = (makeAction "thinking")
  { responses = thinkingResponses
  , checkRules = makeActionRulebook "check thinking" ([] <> map notImplementedRule
    [ "can't do thinking"
    ])
  , carryOutRules = makeActionRulebook "carry out thinking" [ notImplementedRule "standard thinking"  ]
  , reportRules = makeActionRulebook "report thinking"  [ notImplementedRule "standard report thinking"  ]
  }
