
module Yaifl.Std.Actions.Closing where

import Yaifl.Prelude

import Yaifl.Std.Actions.Imports
import Yaifl.Std.Kinds.Openable
import Yaifl.Core.Metadata
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.HasProperty

data ClosingResponses wm =
    CloseReportA
    | CloseReportB
    | CloseReportC
    | CloseUnclosableA
    | CloseAlreadyClosedA
{- data ClosingResponses wm = OR
  { unlessOpenableResponseA :: Response wm (Thing wm)
  , ifAlreadyClosedResponseA :: Response wm (Thing wm)
  , reportClosingResponseA :: Response wm (Thing wm)
  , reportClosingResponseB :: Response wm (Thing wm, Thing wm) -- ^ Actor, thing
  , reportClosingResponseC :: Response wm (Thing wm)
  } -}

closingResponses :: WithPrintingNameOfSomething wm => ClosingResponses wm -> Response wm (Args wm (Thing wm))
closingResponses = \case
  -- say "[We] [close] [the noun]." (A);
  CloseReportA -> Response $ \Args{variables=noun} -> [sayingTell|#{We} #{close} {the noun}.|]
  _ -> error ""

type ClosingAction wm = Action wm (ClosingResponses wm) 'TakesThingParameter (Thing wm)
closingAction :: WMWithProperty wm Openability => WithPrintingNameOfSomething wm => ClosingAction wm
closingAction = (makeAction "closing")
  { understandAs = ["close", "closing"]
  , responses = closingResponses
  , touchableNouns = oneTouchableThing
  , parseArguments = actionOnOneThing
  , checkRules = makeActionRulebook "check closing" ([] <> map notImplementedRule
    [ "can't close unless openable"
    , "can't close what's already closed"
    ])
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

standardReport :: SayableValue (WMText wm) wm => ActionRule wm (ClosingAction wm) (Thing wm)
standardReport = makeRule "standard report closing rule" [] $ \args -> do
  -- if the actor is the player:
  pl <- isPlayer (source args)
  -- if the action is not silent:
  if pl && not (silently . actionOptions $ args)
  then
    -- say "[We] [close] [the noun]." (A);
    sayResponse CloseReportA args
  else
    pass
  rulePass