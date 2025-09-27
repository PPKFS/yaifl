module Yaifl.Std.Actions.TakingInventory
  ( TakingInventoryResponses(..)
  , TakingInventoryRule
  , takingInventoryAction
  , TakingInventoryAction
  , takingInventoryResponses
  ) where

import Yaifl.Std.Actions.Imports

data TakingInventoryResponses wm =
  TakeInventoryEmptyA
  | TakeInventoryStandardA
  | TakeInventoryReportOthersA

takingInventoryResponses :: TakingInventoryResponses wm -> Response wm (Args wm ())
takingInventoryResponses = \case
  _ -> notImplementedResponse "response"

type TakingInventoryAction wm = Action wm (TakingInventoryResponses wm) 'TakesNoParameter ()
type TakingInventoryRule wm = ActionRule wm (TakingInventoryAction wm) ()

takingInventoryAction :: TakingInventoryAction wm
takingInventoryAction = (makeAction "taking inventory")
  { responses = takingInventoryResponses
  , carryOutRules = makeActionRulebook "carry out taking inventory" [ notImplementedRule "print empty inventory", notImplementedRule "print standard inventory"  ]
  , reportRules = makeActionRulebook "report $3 rulebook"  [ notImplementedRule "report other people taking inventory" ]
  }