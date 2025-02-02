module Yaifl.Std.Actions.Showing
  ( ShowingResponses(..)
  , ShowingAction
  , ShowingRule
  , showingAction
  , showingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data ShowingResponses wm =
  FooA

showingResponses :: ShowingResponses wm -> Response wm (Args wm (Thing wm))
showingResponses = \case
  _ -> notImplementedResponse "response"

type ShowingAction wm = Action wm (ShowingResponses wm) 'TakesNoParameter (Thing wm)
type ShowingRule wm = ActionRule wm (ShowingAction wm) (Thing wm)
showingAction :: ShowingAction wm
showingAction = (makeAction "showing")
  { responses = showingResponses
  , checkRules = makeActionRulebook "check showing" ([] <> map notImplementedRule
    [ "can't do showing"
    ])
  , carryOutRules = makeActionRulebook "carry out showing" [ notImplementedRule "standard showing"  ]
  , reportRules = makeActionRulebook "report showing"  [ notImplementedRule "standard report showing"  ]
  }
