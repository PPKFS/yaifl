module Yaifl.Std.Actions.AskingFor
  ( AskingForResponses(..)
  , AskingForAction
  , AskingForRule
  , askingForAction
  , askingForResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data AskingForResponses wm =
  FooA

askingForResponses :: AskingForResponses wm -> Response wm (Args wm (Thing wm))
askingForResponses = \case
  _ -> notImplementedResponse "response"

type AskingForAction wm = Action wm (AskingForResponses wm) 'TakesNoParameter (Thing wm)
type AskingForRule wm = ActionRule wm (AskingForAction wm) (Thing wm)
askingForAction :: AskingForAction wm
askingForAction = (makeAction "askingFor")
  { responses = askingForResponses
  , checkRules = makeActionRulebook "check askingFor" ([] <> map notImplementedRule
    [ "can't do askingFor"
    ])
  , carryOutRules = makeActionRulebook "carry out askingFor" [ notImplementedRule "standard askingFor"  ]
  , reportRules = makeActionRulebook "report askingFor"  [ notImplementedRule "standard report askingFor"  ]
  }
