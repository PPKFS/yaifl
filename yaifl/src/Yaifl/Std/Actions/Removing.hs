module Yaifl.Std.Actions.Removing
  ( RemovingResponses(..)
  , RemovingAction
  , RemovingRule
  , removingAction
  , removingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data RemovingResponses wm =
  RemoveNotInsideA
  | RemoveFromPeopleA

removingResponses :: RemovingResponses wm -> Response wm (Args wm (Thing wm))
removingResponses = \case
  _ -> notImplementedResponse "response"

type RemovingAction wm = Action wm (RemovingResponses wm) 'TakesNoParameter (Thing wm)
type RemovingRule wm = ActionRule wm (RemovingAction wm) (Thing wm)
removingAction :: RemovingAction wm
removingAction = (makeAction "removing")
  { responses = removingResponses
  , checkRules = makeActionRulebook "check removing"
    [ notImplementedRule "can't remove what's not inside"
    , notImplementedRule "can't remove from people"
    , notImplementedRule "convert remove to take"
    , notImplementedRule "can't remove what's not inside"
    , notImplementedRule "can't take components" ]
  }
