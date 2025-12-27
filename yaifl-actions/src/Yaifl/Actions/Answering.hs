module Yaifl.Actions.Answering
  ( AnsweringResponses(..)
  , AnsweringAction
  , AnsweringRule
  , answeringAction
  , answeringResponses
  ) where

import Yaifl.Prelude
import Yaifl.Actions.Imports
import Yaifl.Thing.Kind

data AnsweringResponses wm =
  FooA

answeringResponses :: AnsweringResponses wm -> Response wm (Args wm (Thing wm))
answeringResponses = \case
  _ -> notImplementedResponse "response"

type AnsweringAction wm = Action wm (AnsweringResponses wm) 'TakesNoParameter (Thing wm)
type AnsweringRule wm = ActionRule wm (AnsweringAction wm) (Thing wm)
answeringAction :: AnsweringAction wm
answeringAction = (makeAction "answering")
  { responses = answeringResponses
  , checkRules = makeActionRulebook "check answering" ([] <> map notImplementedRule
    [ "can't do answering"
    ])
  , carryOutRules = makeActionRulebook "carry out answering" [ notImplementedRule "standard answering"  ]
  , reportRules = makeActionRulebook "report answering"  [ notImplementedRule "standard report answering"  ]
  }
