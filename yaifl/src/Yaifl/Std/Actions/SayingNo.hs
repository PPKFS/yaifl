module Yaifl.Std.Actions.SayingNo
  ( SayingNoResponses(..)
  , SayingNoAction
  , SayingNoRule
  , sayingNoAction
  , sayingNoResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data SayingNoResponses wm =
  FooA

sayingNoResponses :: SayingNoResponses wm -> Response wm (Args wm (Thing wm))
sayingNoResponses = \case
  _ -> notImplementedResponse "response"

type SayingNoAction wm = Action wm (SayingNoResponses wm) 'TakesNoParameter (Thing wm)
type SayingNoRule wm = ActionRule wm (SayingNoAction wm) (Thing wm)
sayingNoAction :: SayingNoAction wm
sayingNoAction = (makeAction "sayingNo")
  { responses = sayingNoResponses
  , checkRules = makeActionRulebook "check sayingNo" ([] <> map notImplementedRule
    [ "can't do sayingNo"
    ])
  , carryOutRules = makeActionRulebook "carry out sayingNo" [ notImplementedRule "standard sayingNo"  ]
  , reportRules = makeActionRulebook "report sayingNo"  [ notImplementedRule "standard report sayingNo"  ]
  }
