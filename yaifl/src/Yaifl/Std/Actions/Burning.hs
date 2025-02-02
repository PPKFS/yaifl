module Yaifl.Std.Actions.Burning
  ( BurningResponses(..)
  , BurningAction
  , BurningRule
  , burningAction
  , burningResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data BurningResponses wm =
  FooA

burningResponses :: BurningResponses wm -> Response wm (Args wm (Thing wm))
burningResponses = \case
  _ -> notImplementedResponse "response"

type BurningAction wm = Action wm (BurningResponses wm) 'TakesNoParameter (Thing wm)
type BurningRule wm = ActionRule wm (BurningAction wm) (Thing wm)
burningAction :: BurningAction wm
burningAction = (makeAction "burning")
  { responses = burningResponses
  , checkRules = makeActionRulebook "check burning" ([] <> map notImplementedRule
    [ "can't do burning"
    ])
  , carryOutRules = makeActionRulebook "carry out burning" [ notImplementedRule "standard burning"  ]
  , reportRules = makeActionRulebook "report burning"  [ notImplementedRule "standard report burning"  ]
  }
