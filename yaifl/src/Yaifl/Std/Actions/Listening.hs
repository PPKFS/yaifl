module Yaifl.Std.Actions.Listening
  ( ListeningResponses(..)
  , ListeningAction
  , ListeningRule
  , listeningAction
  , listeningResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data ListeningResponses wm =
  FooA

listeningResponses :: ListeningResponses wm -> Response wm (Args wm (Thing wm))
listeningResponses = \case
  _ -> notImplementedResponse "response"

type ListeningAction wm = Action wm (ListeningResponses wm) 'TakesNoParameter (Thing wm)
type ListeningRule wm = ActionRule wm (ListeningAction wm) (Thing wm)
listeningAction :: ListeningAction wm
listeningAction = (makeAction "listening")
  { responses = listeningResponses
  , checkRules = makeActionRulebook "check listening" ([] <> map notImplementedRule
    [ "can't do listening"
    ])
  , carryOutRules = makeActionRulebook "carry out listening" [ notImplementedRule "standard listening"  ]
  , reportRules = makeActionRulebook "report listening"  [ notImplementedRule "standard report listening"  ]
  }
