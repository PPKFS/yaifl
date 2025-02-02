module Yaifl.Std.Actions.Pushing
  ( PushingResponses(..)
  , PushingAction
  , PushingRule
  , pushingAction
  , pushingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data PushingResponses wm =
  FooA

pushingResponses :: PushingResponses wm -> Response wm (Args wm (Thing wm))
pushingResponses = \case
  _ -> notImplementedResponse "response"

type PushingAction wm = Action wm (PushingResponses wm) 'TakesNoParameter (Thing wm)
type PushingRule wm = ActionRule wm (PushingAction wm) (Thing wm)
pushingAction :: PushingAction wm
pushingAction = (makeAction "pushing")
  { responses = pushingResponses
  , checkRules = makeActionRulebook "check pushing" ([] <> map notImplementedRule
    [ "can't do pushing"
    ])
  , carryOutRules = makeActionRulebook "carry out pushing" [ notImplementedRule "standard pushing"  ]
  , reportRules = makeActionRulebook "report pushing"  [ notImplementedRule "standard report pushing"  ]
  }
