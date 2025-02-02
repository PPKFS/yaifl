module Yaifl.Std.Actions.Asking
  ( AskingResponses(..)
  , AskingAction
  , AskingRule
  , askingAction
  , askingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data AskingResponses wm =
  FooA

askingResponses :: AskingResponses wm -> Response wm (Args wm (Thing wm))
askingResponses = \case
  _ -> notImplementedResponse "response"

type AskingAction wm = Action wm (AskingResponses wm) 'TakesNoParameter (Thing wm)
type AskingRule wm = ActionRule wm (AskingAction wm) (Thing wm)
askingAction :: AskingAction wm
askingAction = (makeAction "asking")
  { responses = askingResponses
  , checkRules = makeActionRulebook "check asking" ([] <> map notImplementedRule
    [ "can't do asking"
    ])
  , carryOutRules = makeActionRulebook "carry out asking" [ notImplementedRule "standard asking"  ]
  , reportRules = makeActionRulebook "report asking"  [ notImplementedRule "standard report asking"  ]
  }
