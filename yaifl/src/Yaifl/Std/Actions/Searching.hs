module Yaifl.Std.Actions.Searching
  ( SearchingResponses(..)
  , SearchingAction
  , SearchingRule
  , searchingAction
  , searchingResponses
  ) where

import Yaifl.Prelude
import Yaifl.Std.Actions.Imports
import Yaifl.Core.Kinds.Thing

data SearchingResponses wm =
  SearchContainerSupporterA
  | SearchClosedOpaqueA
  | SearchContainersA
  | SearchContainersB
  | SearchSupportersA
  | SearchSupportersB
  | SearchOthersA


searchingResponses :: SearchingResponses wm -> Response wm (Args wm (Thing wm))
searchingResponses = \case
  _ -> notImplementedResponse "response"

type SearchingAction wm = Action wm (SearchingResponses wm) 'TakesThingParameter (Thing wm)
type SearchingRule wm = ActionRule wm (SearchingAction wm) (Thing wm)
searchingAction :: SearchingAction wm
searchingAction = (makeAction "searching")
  { responses = searchingResponses
  , checkRules = makeActionRulebook "check searching" ([] <> map notImplementedRule
    [ "can't search unless container or supporter"
    , "can't search closed opaque containers"
    ])
  , carryOutRules = makeActionRulebook "carry out searching" [ ]
  , reportRules = makeActionRulebook "report searching"  [ notImplementedRule "standard searching containers", notImplementedRule "standard searching supporters", notImplementedRule "other people searching"  ]
  }
