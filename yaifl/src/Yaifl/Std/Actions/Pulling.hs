module Yaifl.Std.Actions.Pulling
  ( PullingResponses(..)
  , PullingAction
  , PullingRule
  , pullingAction
  , pullingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data PullingResponses wm =
  FooA

pullingResponses :: PullingResponses wm -> Response wm (Args wm (Thing wm))
pullingResponses = \case
  _ -> notImplementedResponse "response"

type PullingAction wm = Action wm (PullingResponses wm) 'TakesNoParameter (Thing wm)
type PullingRule wm = ActionRule wm (PullingAction wm) (Thing wm)
pullingAction :: PullingAction wm
pullingAction = (makeAction "pulling")
  { responses = pullingResponses
  , checkRules = makeActionRulebook "check pulling" ([] <> map notImplementedRule
    [ "can't do pulling"
    ])
  , carryOutRules = makeActionRulebook "carry out pulling" [ notImplementedRule "standard pulling"  ]
  , reportRules = makeActionRulebook "report pulling"  [ notImplementedRule "standard report pulling"  ]
  }
