module Yaifl.Std.Actions.Kissing
  ( KissingResponses(..)
  , KissingAction
  , KissingRule
  , kissingAction
  , kissingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data KissingResponses wm =
  FooA

kissingResponses :: KissingResponses wm -> Response wm (Args wm (Thing wm))
kissingResponses = \case
  _ -> notImplementedResponse "response"

type KissingAction wm = Action wm (KissingResponses wm) 'TakesNoParameter (Thing wm)
type KissingRule wm = ActionRule wm (KissingAction wm) (Thing wm)
kissingAction :: KissingAction wm
kissingAction = (makeAction "kissing")
  { responses = kissingResponses
  , checkRules = makeActionRulebook "check kissing" ([] <> map notImplementedRule
    [ "can't do kissing"
    ])
  , carryOutRules = makeActionRulebook "carry out kissing" [ notImplementedRule "standard kissing"  ]
  , reportRules = makeActionRulebook "report kissing"  [ notImplementedRule "standard report kissing"  ]
  }
