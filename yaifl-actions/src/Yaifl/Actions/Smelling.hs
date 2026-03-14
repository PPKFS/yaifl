module Yaifl.Actions.Smelling
  ( SmellingResponses(..)
  , SmellingAction
  , SmellingRule
  , smellingAction
  , smellingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Actions.Imports
import Yaifl.Thing.Kind

data SmellingResponses wm =
  FooA

smellingResponses :: SmellingResponses wm -> Response wm (Args wm (Thing wm))
smellingResponses = \case
  _ -> notImplementedResponse "response"

type SmellingAction wm = Action wm (SmellingResponses wm) 'TakesNoParameter (Thing wm)
type SmellingRule wm = ActionRule wm (SmellingAction wm) (Thing wm)
smellingAction :: SmellingAction wm
smellingAction = (makeAction "smelling")
  { responses = smellingResponses
  , checkRules = makeActionRulebook "check smelling" ([] <> map notImplementedRule
    [ "can't do smelling"
    ])
  , carryOutRules = makeActionRulebook "carry out smelling" [ notImplementedRule "standard smelling"  ]
  , reportRules = makeActionRulebook "report smelling"  [ notImplementedRule "standard report smelling"  ]
  }
