module Yaifl.Std.Actions.Eating
  ( EatingResponses(..)
  , EatingAction
  , EatingRule
  , eatingAction
  , eatingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data EatingResponses wm =
  EatInedibleA
  | EatClothingA
  | EatPeoplesFoodA
  | EatReportA
  | EatReportB

eatingResponses :: EatingResponses wm -> Response wm (Args wm (Thing wm))
eatingResponses = \case
  _ -> notImplementedResponse "response"

type EatingAction wm = Action wm (EatingResponses wm) 'TakesNoParameter (Thing wm)
type EatingRule wm = ActionRule wm (EatingAction wm) (Thing wm)

eatingAction :: EatingAction wm
eatingAction = (makeAction "eating")
  { responses = eatingResponses
  , checkRules = makeActionRulebook "check eating" ([] <> map notImplementedRule
    [ "can't eat unless edible"
    , "can't eat clothing without removing it first"
    , "can't eat other people's food"
    , "can't eat portable food without carrying it"
    ])
  , carryOutRules = makeActionRulebook "carry out eating" [ notImplementedRule "standard eating"  ]
  , reportRules = makeActionRulebook "report eating"  [ notImplementedRule "standard report eating"  ]
  }
