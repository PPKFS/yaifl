module Yaifl.Std.Actions.Telling
  ( TellingResponses(..)
  , TellingAction
  , TellingRule
  , tellingAction
  , tellingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data TellingResponses wm =
  FooA

tellingResponses :: TellingResponses wm -> Response wm (Args wm (Thing wm))
tellingResponses = \case
  _ -> notImplementedResponse "response"

type TellingAction wm = Action wm (TellingResponses wm) 'TakesNoParameter (Thing wm)
type TellingRule wm = ActionRule wm (TellingAction wm) (Thing wm)
tellingAction :: TellingAction wm
tellingAction = (makeAction "telling")
  { responses = tellingResponses
  , checkRules = makeActionRulebook "check telling" ([] <> map notImplementedRule
    [ "can't do telling"
    ])
  , carryOutRules = makeActionRulebook "carry out telling" [ notImplementedRule "standard telling"  ]
  , reportRules = makeActionRulebook "report telling"  [ notImplementedRule "standard report telling"  ]
  }
