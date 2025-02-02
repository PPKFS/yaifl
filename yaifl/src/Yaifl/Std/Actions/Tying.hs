module Yaifl.Std.Actions.Tying
  ( TyingResponses(..)
  , TyingAction
  , TyingRule
  , tyingAction
  , tyingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data TyingResponses wm =
  FooA

tyingResponses :: TyingResponses wm -> Response wm (Args wm (Thing wm))
tyingResponses = \case
  _ -> notImplementedResponse "response"

type TyingAction wm = Action wm (TyingResponses wm) 'TakesNoParameter (Thing wm)
type TyingRule wm = ActionRule wm (TyingAction wm) (Thing wm)
tyingAction :: TyingAction wm
tyingAction = (makeAction "tying")
  { responses = tyingResponses
  , checkRules = makeActionRulebook "check tying" ([] <> map notImplementedRule
    [ "can't do tying"
    ])
  , carryOutRules = makeActionRulebook "carry out tying" [ notImplementedRule "standard tying"  ]
  , reportRules = makeActionRulebook "report tying"  [ notImplementedRule "standard report tying"  ]
  }
