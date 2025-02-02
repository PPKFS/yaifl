
module Yaifl.Std.Actions.Opening where

import Yaifl.Std.Actions.Imports
import Yaifl.Prelude
import Yaifl.Std.Kinds.Openable
import Yaifl.Core.Metadata
import Yaifl.Core.Kinds.Thing

data OpeningResponses =
  OpenNotOpenableResponseA
  | OpenLockedResponseA
  | OpenAlreadyOpenResponseA
  | OpenRevealNewInteriorResponseA
  | OpenReportResponseA
  | OpenReportResponseB
  | OpenReportResponseC
  deriving stock (Generic)

type OpeningAction wm = Action wm OpeningResponses 'TakesThingParameter (Thing wm)

openingResponses :: WithPrintingNameOfSomething wm => OpeningResponses -> Response wm (Args wm (Thing wm))
openingResponses = \case
  -- say "[We] [open] [the noun]." (A);
  OpenReportResponseA -> Response $ \Args{variables=noun} -> [sayingTell|#{We} #{open} {the noun}.|]
  _ -> error ""

openingAction :: WithPrintingNameOfSomething wm => WMWithProperty wm Openability => OpeningAction wm
openingAction = (makeAction "opening")
  { understandAs = ["open", "opening"]
  , responses = openingResponses
  , parseArguments = actionOnOneThing
  , checkRules = makeActionRulebook "check opening"
      [ cantOpenUnlessOpenable
      , cantOpenIfLocked
      , cantOpenIfOpen
      ]
  , carryOutRules = makeActionRulebook "carry out opening rulebook" [ standardOpening ]
  , reportRules = makeActionRulebook "report opening rulebook" [ revealNewInterior, standardReport ]
  }

cantOpenUnlessOpenable :: ActionRule wm (OpeningAction wm) (Thing wm)
cantOpenUnlessOpenable = notImplementedRule "can't open nonopenable things"

cantOpenIfLocked :: ActionRule wm (OpeningAction wm) (Thing wm)
cantOpenIfLocked = notImplementedRule "can't open nonopenable things"

cantOpenIfOpen :: ActionRule wm (OpeningAction wm) (Thing wm)
cantOpenIfOpen = notImplementedRule "can't open nonopenable things"

standardReport :: SayableValue (WMText wm) wm => ActionRule wm (OpeningAction wm) (Thing wm)
standardReport = makeRule "standard report opening rule" [] $ \args -> do
  -- if the actor is the player:
  pl <- isPlayer (source args)
  -- if the action is not silent:
  if pl && not (silently . actionOptions $ args)
  then
    -- say "[We] [open] [the noun]." (A);
    sayResponse OpenReportResponseA args
  else
    pass

  {-
    otherwise if the player can see the actor:
        say "[The actor] [open] [the noun]." (B);
    otherwise:
        say "[The noun] [open]." (C);
  -}
  rulePass

revealNewInterior :: ActionRule wm (OpeningAction wm) (Thing wm)
revealNewInterior = notImplementedRule "revealing new interiors"

standardOpening :: WMWithProperty wm Openability => ActionRule wm (OpeningAction wm) (Thing wm)
standardOpening = makeRule "standard opening rule" [] $ \thing -> do
  -- now the noun is open.
  openIt (variables thing)
  rulePass
