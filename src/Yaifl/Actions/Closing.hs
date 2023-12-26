
module Yaifl.Actions.Closing where

import Yaifl.Actions.Action
import Solitude
import Yaifl.Rules.Args
import Yaifl.Text.Responses
import Yaifl.Model.Object
import Yaifl.Actions.Examining

type ClosingResponses = ()
{- data ClosingResponses wm = OR
  { unlessOpenableResponseA :: Response wm (Thing wm)
  , ifAlreadyClosedResponseA :: Response wm (Thing wm)
  , reportClosingResponseA :: Response wm (Thing wm)
  , reportClosingResponseB :: Response wm (Thing wm, Thing wm) -- ^ Actor, thing
  , reportClosingResponseC :: Response wm (Thing wm)
  } -}

type ClosingAction wm = Action wm ClosingResponses 'TakesThingParameter (Thing wm)
closingAction :: ClosingAction wm
closingAction = Action
  { name = "closing"
  , understandAs = ["close", "closing"]
  , matches = []
  , responses = (\() -> error "")
  , parseArguments = actionOnOneThing
  , beforeRules = makeActionRulebook "before closing rulebook" []
  , insteadRules = makeActionRulebook "instead of closing rulebook" []
  , checkRules = makeActionRulebook "check closing rulebook" []
  , carryOutRules = makeActionRulebook "carry out closing rulebook" []
  , reportRules = makeActionRulebook "report closing rulebook" []
  }