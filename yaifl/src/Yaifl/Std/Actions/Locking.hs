module Yaifl.Std.Actions.Locking
  ( LockingResponses(..)
  , LockingAction
  , LockingRule
  , lockingAction
  , lockingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data LockingResponses wm =
  LockWithoutKeyA
  | LockAlreadyLockedA
  | LockOpenA
  | LockWrongKeyA
  | LockReportA
  | LockReportB

lockingResponses :: LockingResponses wm -> Response wm (Args wm (Thing wm))
lockingResponses = \case
  _ -> notImplementedResponse "response"

type LockingAction wm = Action wm (LockingResponses wm) 'TakesNoParameter (Thing wm)
type LockingRule wm = ActionRule wm (LockingAction wm) (Thing wm)
lockingAction :: LockingAction wm
lockingAction = (makeAction "locking")
  { responses = lockingResponses
  , checkRules = makeActionRulebook "check locking" ([] <> map notImplementedRule
    [ "can't lock without holding the key"
    , "can't lock what's already locked"
    , "can't lock what's open"
    , "can't lock without the correct key"
    ])
  , carryOutRules = makeActionRulebook "carry out locking" [ notImplementedRule "standard locking"  ]
  , reportRules = makeActionRulebook "report locking"  [ notImplementedRule "standard report locking"  ]
  }
