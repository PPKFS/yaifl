
module Yaifl.Game.Actions.Closing where

import Yaifl.Model.Action
import Yaifl.Prelude
import Yaifl.Model.Actions.Args
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Responses
import Yaifl.Model.Metadata
import Yaifl.Text.SayQQ
import Yaifl.Text.Say
import Yaifl.Model.Kinds.Thing

data ClosingResponses = ReportClosingResponseA
{- data ClosingResponses wm = OR
  { unlessOpenableResponseA :: Response wm (Thing wm)
  , ifAlreadyClosedResponseA :: Response wm (Thing wm)
  , reportClosingResponseA :: Response wm (Thing wm)
  , reportClosingResponseB :: Response wm (Thing wm, Thing wm) -- ^ Actor, thing
  , reportClosingResponseC :: Response wm (Thing wm)
  } -}

closingResponses :: WithPrintingNameOfSomething wm => ClosingResponses -> Response wm (Args wm (Thing wm))
closingResponses = \case
  -- say "[We] [close] [the noun]." (A);
  ReportClosingResponseA -> Response $ \Args{variables=noun} -> [sayingTell|#{We} #{close} {the noun}.|]

type ClosingAction wm = Action wm ClosingResponses 'TakesThingParameter (Thing wm)
closingAction :: WMWithProperty wm Openability => WithPrintingNameOfSomething wm => ClosingAction wm
closingAction = (makeAction "closing")
  { understandAs = ["close", "closing"]
  , responses = closingResponses
  , parseArguments = actionOnOneThing
  , carryOutRules = makeActionRulebook "carry out closing rulebook" [ standardClose ]
  , reportRules = makeActionRulebook "report closing rulebook" [ standardReport ]
  }

cantCloseUnlessOpenable :: ActionRule wm (ClosingAction wm) (Thing wm)
cantCloseUnlessOpenable = notImplementedRule "can't close nonopenable things"

cantCloseIfClosed :: ActionRule wm (ClosingAction wm) (Thing wm)
cantCloseIfClosed = notImplementedRule "can't close cloed things"

standardClose :: WMWithProperty wm Openability => ActionRule wm (ClosingAction wm) (Thing wm)
standardClose = makeRule "standard closing rule" [] $ \Args{variables=thing} -> do
  -- now the noun is closed.
  closeIt thing
  rulePass

standardReport :: ActionRule wm (ClosingAction wm) (Thing wm)
standardReport = makeRule "standard report closing rule" [] $ \args -> do
  -- if the actor is the player:
  pl <- isPlayer (source args)
  -- if the action is not silent:
  if pl && not (silently . actionOptions $ args)
  then
    -- say "[We] [open] [the noun]." (A);
    sayResponse ReportClosingResponseA args
  else
    pass
  rulePass