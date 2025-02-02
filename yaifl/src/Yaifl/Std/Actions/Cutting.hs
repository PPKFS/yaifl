module Yaifl.Std.Actions.Cutting
  ( CuttingResponses(..)
  , CuttingAction
  , CuttingRule
  , cuttingAction
  , cuttingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data CuttingResponses wm =
  FooA

cuttingResponses :: CuttingResponses wm -> Response wm (Args wm (Thing wm))
cuttingResponses = \case
  _ -> notImplementedResponse "response"

type CuttingAction wm = Action wm (CuttingResponses wm) 'TakesNoParameter (Thing wm)
type CuttingRule wm = ActionRule wm (CuttingAction wm) (Thing wm)
cuttingAction :: CuttingAction wm
cuttingAction = (makeAction "cutting")
  { responses = cuttingResponses
  , checkRules = makeActionRulebook "check cutting" ([] <> map notImplementedRule
    [ "can't do cutting"
    ])
  , carryOutRules = makeActionRulebook "carry out cutting" [ notImplementedRule "standard cutting"  ]
  , reportRules = makeActionRulebook "report cutting"  [ notImplementedRule "standard report cutting"  ]
  }
