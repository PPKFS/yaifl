module Yaifl.Actions.Attacking
  ( AttackingResponses(..)
  , AttackingAction
  , AttackingRule
  , attackingAction
  , attackingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Actions.Imports
import Yaifl.Thing.Kind

data AttackingResponses wm =
  FooA

attackingResponses :: AttackingResponses wm -> Response wm (Args wm (Thing wm))
attackingResponses = \case
  _ -> notImplementedResponse "response"

type AttackingAction wm = Action wm (AttackingResponses wm) 'TakesNoParameter (Thing wm)
type AttackingRule wm = ActionRule wm (AttackingAction wm) (Thing wm)
attackingAction :: AttackingAction wm
attackingAction = (makeAction "attacking")
  { responses = attackingResponses
  , checkRules = makeActionRulebook "check attacking" ([] <> map notImplementedRule
    [ "can't do attacking"
    ])
  , carryOutRules = makeActionRulebook "carry out attacking" [ notImplementedRule "standard attacking"  ]
  , reportRules = makeActionRulebook "report attacking"  [ notImplementedRule "standard report attacking"  ]
  }
