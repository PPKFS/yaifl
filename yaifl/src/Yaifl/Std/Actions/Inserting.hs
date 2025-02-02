module Yaifl.Std.Actions.Inserting
  ( InsertingResponses(..)
  , InsertingAction
  , InsertingRule
  , insertingAction
  , insertingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data InsertingResponses wm =
  InsertAlreadyInsertedA
  | InsertItselfA
  | InsertClosedContainersA
  | InsertNotContainerA
  | InsertClothesWornA
  | InsertCapacityA
  | InsertConciseReportA
  | InsertStandardReportA

insertingResponses :: InsertingResponses wm -> Response wm (Args wm (Thing wm))
insertingResponses = \case
  _ -> notImplementedResponse "response"

type InsertingAction wm = Action wm (InsertingResponses wm) 'TakesNoParameter (Thing wm)
type InsertingRule wm = ActionRule wm (InsertingAction wm) (Thing wm)
insertingAction :: InsertingAction wm
insertingAction = (makeAction "inserting")
  { responses = insertingResponses
  , checkRules = makeActionRulebook "check inserting" ([] <> map notImplementedRule
    [ "convert insert to drop where possible"
    , "can't insert what's already inserted"
    , "can't insert into itself"
    , "can't insert what's not held"
    , "can't insert into closed containers"
    , "can't insert into what's not a container"
    , "can't insert clothes being worn"
    , "can't insert if this exceeds carrying capacity"
    ])
  , carryOutRules = makeActionRulebook "carry out inserting" [ notImplementedRule "standard inserting"  ]
  , reportRules = makeActionRulebook "report inserting"  [ notImplementedRule "concise report inserting", notImplementedRule "standard report inserting" ]
  }
