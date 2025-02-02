module Yaifl.Std.Actions.Jumping
  ( JumpingResponses(..)
  , JumpingAction
  , JumpingRule
  , jumpingAction
  , jumpingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data JumpingResponses wm =
  FooA

jumpingResponses :: JumpingResponses wm -> Response wm (Args wm (Thing wm))
jumpingResponses = \case
  _ -> notImplementedResponse "response"

type JumpingAction wm = Action wm (JumpingResponses wm) 'TakesNoParameter (Thing wm)
type JumpingRule wm = ActionRule wm (JumpingAction wm) (Thing wm)
jumpingAction :: JumpingAction wm
jumpingAction = (makeAction "jumping")
  { responses = jumpingResponses
  , checkRules = makeActionRulebook "check jumping" ([] <> map notImplementedRule
    [ "can't do jumping"
    ])
  , carryOutRules = makeActionRulebook "carry out jumping" [ notImplementedRule "standard jumping"  ]
  , reportRules = makeActionRulebook "report jumping"  [ notImplementedRule "standard report jumping"  ]
  }
