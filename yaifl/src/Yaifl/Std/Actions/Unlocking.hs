module Yaifl.Std.Actions.Unlocking
  ( UnlockingResponses(..)
  , UnlockingAction
  , UnlockingRule
  , unlockingAction
  , unlockingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data UnlockingResponses wm =
  UnlockWithoutLockA
  | UnlockAlreadyUnlockedA
  | UnlockIncorrectKeyA
  | UnlockReportA
  | UnlockReportB


unlockingResponses :: UnlockingResponses wm -> Response wm (Args wm (Thing wm))
unlockingResponses = \case
  _ -> notImplementedResponse "response"

type UnlockingAction wm = Action wm (UnlockingResponses wm) 'TakesNoParameter (Thing wm)
type UnlockingRule wm = ActionRule wm (UnlockingAction wm) (Thing wm)
unlockingAction :: UnlockingAction wm
unlockingAction = (makeAction "unlocking")
  { responses = unlockingResponses
  , checkRules = makeActionRulebook "check unlocking" ([] <> map notImplementedRule
    [ "can't unlock without holding the key"
    , "can't unlock without a lock"
    , "can't unlock what's already unlocked"
    , "can't unlock without correct key"
    ])
  , carryOutRules = makeActionRulebook "carry out unlocking" [ notImplementedRule "standard unlocking"  ]
  , reportRules = makeActionRulebook "report unlocking"  [ notImplementedRule "standard report unlocking"  ]
  }
