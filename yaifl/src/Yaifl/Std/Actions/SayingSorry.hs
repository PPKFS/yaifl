module Yaifl.Std.Actions.SayingSorry
  ( SayingSorryResponses(..)
  , SayingSorryAction
  , SayingSorryRule
  , sayingSorryAction
  , sayingSorryResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data SayingSorryResponses wm =
  FooA

sayingSorryResponses :: SayingSorryResponses wm -> Response wm (Args wm (Thing wm))
sayingSorryResponses = \case
  _ -> notImplementedResponse "response"

type SayingSorryAction wm = Action wm (SayingSorryResponses wm) 'TakesNoParameter (Thing wm)
type SayingSorryRule wm = ActionRule wm (SayingSorryAction wm) (Thing wm)
sayingSorryAction :: SayingSorryAction wm
sayingSorryAction = (makeAction "sayingSorry")
  { responses = sayingSorryResponses
  , checkRules = makeActionRulebook "check sayingSorry" ([] <> map notImplementedRule
    [ "can't do sayingSorry"
    ])
  , carryOutRules = makeActionRulebook "carry out sayingSorry" [ notImplementedRule "standard sayingSorry"  ]
  , reportRules = makeActionRulebook "report sayingSorry"  [ notImplementedRule "standard report sayingSorry"  ]
  }
