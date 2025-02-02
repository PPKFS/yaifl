module Yaifl.Std.Actions.Squeezing
  ( SqueezingResponses(..)
  , SqueezingAction
  , SqueezingRule
  , squeezingAction
  , squeezingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data SqueezingResponses wm =
  FooA

squeezingResponses :: SqueezingResponses wm -> Response wm (Args wm (Thing wm))
squeezingResponses = \case
  _ -> notImplementedResponse "response"

type SqueezingAction wm = Action wm (SqueezingResponses wm) 'TakesNoParameter (Thing wm)
type SqueezingRule wm = ActionRule wm (SqueezingAction wm) (Thing wm)
squeezingAction :: SqueezingAction wm
squeezingAction = (makeAction "squeezing")
  { responses = squeezingResponses
  , checkRules = makeActionRulebook "check squeezing" ([] <> map notImplementedRule
    [ "can't do squeezing"
    ])
  , carryOutRules = makeActionRulebook "carry out squeezing" [ notImplementedRule "standard squeezing"  ]
  , reportRules = makeActionRulebook "report squeezing"  [ notImplementedRule "standard report squeezing"  ]
  }
