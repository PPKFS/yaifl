module Yaifl.Actions.Buying
  ( BuyingResponses(..)
  , BuyingAction
  , BuyingRule
  , buyingAction
  , buyingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Actions.Imports
import Yaifl.Thing.Kind

data BuyingResponses wm =
  FooA

buyingResponses :: BuyingResponses wm -> Response wm (Args wm (Thing wm))
buyingResponses = \case
  _ -> notImplementedResponse "response"

type BuyingAction wm = Action wm (BuyingResponses wm) 'TakesNoParameter (Thing wm)
type BuyingRule wm = ActionRule wm (BuyingAction wm) (Thing wm)
buyingAction :: BuyingAction wm
buyingAction = (makeAction "buying")
  { responses = buyingResponses
  , checkRules = makeActionRulebook "check buying" ([] <> map notImplementedRule
    [ "can't do buying"
    ])
  , carryOutRules = makeActionRulebook "carry out buying" [ notImplementedRule "standard buying"  ]
  , reportRules = makeActionRulebook "report buying"  [ notImplementedRule "standard report buying"  ]
  }
