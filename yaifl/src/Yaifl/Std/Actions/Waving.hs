module Yaifl.Std.Actions.Waving
  ( WavingResponses(..)
  , WavingAction
  , WavingRule
  , wavingAction
  , wavingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data WavingResponses wm =
  FooA

wavingResponses :: WavingResponses wm -> Response wm (Args wm (Thing wm))
wavingResponses = \case
  _ -> notImplementedResponse "response"

type WavingAction wm = Action wm (WavingResponses wm) 'TakesNoParameter (Thing wm)
type WavingRule wm = ActionRule wm (WavingAction wm) (Thing wm)
wavingAction :: WavingAction wm
wavingAction = (makeAction "waving")
  { responses = wavingResponses
  , checkRules = makeActionRulebook "check waving" ([] <> map notImplementedRule
    [ "can't do waving"
    ])
  , carryOutRules = makeActionRulebook "carry out waving" [ notImplementedRule "standard waving"  ]
  , reportRules = makeActionRulebook "report waving"  [ notImplementedRule "standard report waving"  ]
  }
