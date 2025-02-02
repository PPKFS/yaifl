module Yaifl.Std.Actions.Drinking
  ( DrinkingResponses(..)
  , DrinkingAction
  , DrinkingRule
  , drinkingAction
  , drinkingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data DrinkingResponses wm =
  FooA

drinkingResponses :: DrinkingResponses wm -> Response wm (Args wm (Thing wm))
drinkingResponses = \case
  _ -> notImplementedResponse "response"

type DrinkingAction wm = Action wm (DrinkingResponses wm) 'TakesNoParameter (Thing wm)
type DrinkingRule wm = ActionRule wm (DrinkingAction wm) (Thing wm)
drinkingAction :: DrinkingAction wm
drinkingAction = (makeAction "drinking")
  { responses = drinkingResponses
  , checkRules = makeActionRulebook "check drinking" ([] <> map notImplementedRule
    [ "can't do drinking"
    ])
  , carryOutRules = makeActionRulebook "carry out drinking" [ notImplementedRule "standard drinking"  ]
  , reportRules = makeActionRulebook "report drinking"  [ notImplementedRule "standard report drinking"  ]
  }
