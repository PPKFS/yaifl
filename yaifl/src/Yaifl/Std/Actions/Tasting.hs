module Yaifl.Std.Actions.Tasting
  ( TastingResponses(..)
  , TastingAction
  , TastingRule
  , tastingAction
  , tastingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data TastingResponses wm =
  FooA

tastingResponses :: TastingResponses wm -> Response wm (Args wm (Thing wm))
tastingResponses = \case
  _ -> notImplementedResponse "response"

type TastingAction wm = Action wm (TastingResponses wm) 'TakesNoParameter (Thing wm)
type TastingRule wm = ActionRule wm (TastingAction wm) (Thing wm)
tastingAction :: TastingAction wm
tastingAction = (makeAction "tasting")
  { responses = tastingResponses
  , checkRules = makeActionRulebook "check tasting" ([] <> map notImplementedRule
    [ "can't do tasting"
    ])
  , carryOutRules = makeActionRulebook "carry out tasting" [ notImplementedRule "standard tasting"  ]
  , reportRules = makeActionRulebook "report tasting"  [ notImplementedRule "standard report tasting"  ]
  }
