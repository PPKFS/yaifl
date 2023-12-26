
module Yaifl.Actions.Opening where

import Yaifl.Actions.Action
import Solitude
import Yaifl.Rules.Args
import Yaifl.Text.Responses
import Yaifl.Model.Object
import Yaifl.Actions.Examining

{- data OpeningResponses wm = OR
  { unlessOpenableResponseA :: Response wm (Thing wm)
  , cantOpenLockedResponseA :: Response wm (Thing wm)
  , ifAlreadyOpenResponseA :: Response wm (Thing wm)
  , revealNewInteriorResponseA :: Response wm (Thing wm)
  , reportOpeningResponseA :: Response wm (Thing wm)
  , reportOpeningResponseB :: Response wm (Thing wm, Thing wm)
  , reportOpeningResponseC :: Response wm (Thing wm)
  } -}

type OpeningResponses = ()

type OpeningAction wm = Action wm OpeningResponses 'TakesThingParameter (Thing wm)
openingAction :: OpeningAction wm
openingAction = Action
  { name = "opening"
  , understandAs = ["open", "opening"]
  , matches = []
  , responses = (\() -> error "")
  , parseArguments = actionOnOneThing
  , beforeRules = makeActionRulebook "before opening rulebook" []
  , insteadRules = makeActionRulebook "instead of opening rulebook" []
  , checkRules = makeActionRulebook "check opening rulebook" []
  , carryOutRules = makeActionRulebook "carry out opening rulebook" []
  , reportRules = makeActionRulebook "report opening rulebook" []
  }