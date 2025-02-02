module Yaifl.Std.Actions.SayingYes
  ( SayingYesResponses(..)
  , SayingYesAction
  , SayingYesRule
  , sayingYesAction
  , sayingYesResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data SayingYesResponses wm =
  FooA

sayingYesResponses :: SayingYesResponses wm -> Response wm (Args wm (Thing wm))
sayingYesResponses = \case
  _ -> notImplementedResponse "response"

type SayingYesAction wm = Action wm (SayingYesResponses wm) 'TakesNoParameter (Thing wm)
type SayingYesRule wm = ActionRule wm (SayingYesAction wm) (Thing wm)
sayingYesAction :: SayingYesAction wm
sayingYesAction = (makeAction "sayingYes")
  { responses = sayingYesResponses
  , checkRules = makeActionRulebook "check sayingYes" ([] <> map notImplementedRule
    [ "can't do sayingYes"
    ])
  , carryOutRules = makeActionRulebook "carry out sayingYes" [ notImplementedRule "standard sayingYes"  ]
  , reportRules = makeActionRulebook "report sayingYes"  [ notImplementedRule "standard report sayingYes"  ]
  }
