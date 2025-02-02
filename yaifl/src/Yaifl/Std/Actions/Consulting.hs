module Yaifl.Std.Actions.Consulting
  ( ConsultingResponses(..)
  , ConsultingAction
  , ConsultingRule
  , consultingAction
  , consultingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data ConsultingResponses wm =
  ConsultBlockA
  | ConsultBlockB

consultingResponses :: ConsultingResponses wm -> Response wm (Args wm (Thing wm))
consultingResponses = \case
  _ -> notImplementedResponse "response"

type ConsultingAction wm = Action wm (ConsultingResponses wm) 'TakesNoParameter (Thing wm)
type ConsultingRule wm = ActionRule wm (ConsultingAction wm) (Thing wm)
consultingAction :: ConsultingAction wm
consultingAction = (makeAction "consulting")
  { responses = consultingResponses
  , reportRules = makeActionRulebook "report consulting"  [ notImplementedRule "block consulting"  ]
  }
