module Yaifl.Std.Actions.WavingHands
  ( WavingHandsResponses(..)
  , WavingHandsAction
  , WavingHandsRule
  , wavingHandsAction
  , wavingHandsResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data WavingHandsResponses wm =
  FooA

wavingHandsResponses :: WavingHandsResponses wm -> Response wm (Args wm (Thing wm))
wavingHandsResponses = \case
  _ -> notImplementedResponse "response"

type WavingHandsAction wm = Action wm (WavingHandsResponses wm) 'TakesNoParameter (Thing wm)
type WavingHandsRule wm = ActionRule wm (WavingHandsAction wm) (Thing wm)
wavingHandsAction :: WavingHandsAction wm
wavingHandsAction = (makeAction "wavingHands")
  { responses = wavingHandsResponses
  , checkRules = makeActionRulebook "check wavingHands" ([] <> map notImplementedRule
    [ "can't do wavingHands"
    ])
  , carryOutRules = makeActionRulebook "carry out wavingHands" [ notImplementedRule "standard wavingHands"  ]
  , reportRules = makeActionRulebook "report wavingHands"  [ notImplementedRule "standard report wavingHands"  ]
  }
