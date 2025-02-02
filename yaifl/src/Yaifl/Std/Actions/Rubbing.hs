module Yaifl.Std.Actions.Rubbing
  ( RubbingResponses(..)
  , RubbingAction
  , RubbingRule
  , rubbingAction
  , rubbingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data RubbingResponses wm =
  FooA

rubbingResponses :: RubbingResponses wm -> Response wm (Args wm (Thing wm))
rubbingResponses = \case
  _ -> notImplementedResponse "response"

type RubbingAction wm = Action wm (RubbingResponses wm) 'TakesNoParameter (Thing wm)
type RubbingRule wm = ActionRule wm (RubbingAction wm) (Thing wm)
rubbingAction :: RubbingAction wm
rubbingAction = (makeAction "rubbing")
  { responses = rubbingResponses
  , checkRules = makeActionRulebook "check rubbing" ([] <> map notImplementedRule
    [ "can't do rubbing"
    ])
  , carryOutRules = makeActionRulebook "carry out rubbing" [ notImplementedRule "standard rubbing"  ]
  , reportRules = makeActionRulebook "report rubbing"  [ notImplementedRule "standard report rubbing"  ]
  }
