module Yaifl.Std.Actions.Giving
  ( GivingResponses(..)
  , GivingAction
  , GivingRule
  , givingAction
  , givingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data GivingResponses wm =
  FooA

givingResponses :: GivingResponses wm -> Response wm (Args wm (Thing wm))
givingResponses = \case
  _ -> notImplementedResponse "response"

type GivingAction wm = Action wm (GivingResponses wm) 'TakesNoParameter (Thing wm)
type GivingRule wm = ActionRule wm (GivingAction wm) (Thing wm)
givingAction :: GivingAction wm
givingAction = (makeAction "giving")
  { responses = givingResponses
  , checkRules = makeActionRulebook "check giving" ([] <> map notImplementedRule
    [ "can't do giving"
    ])
  , carryOutRules = makeActionRulebook "carry out giving" [ notImplementedRule "standard giving"  ]
  , reportRules = makeActionRulebook "report giving"  [ notImplementedRule "standard report giving"  ]
  }
