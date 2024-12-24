module Yaifl.Game.Actions.Taking where

import Yaifl.Model.Action
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.Kinds.Thing
import Yaifl.Text.SayQQ
import Yaifl.Prelude
import Yaifl.Text.Say
import Yaifl.Game.Move
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Model.Query (getEnclosingMaybe)
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Tag
import Yaifl.Model.Entity

type TakingAction wm = Action wm () 'TakesThingParameter (Thing wm)
takingAction :: WMWithProperty wm Enclosing => WithPrintingNameOfSomething wm => TakingAction wm
takingAction = (makeAction "taking")
  { name = "taking"
  , understandAs = ["take", "get", "grab"]
  , parseArguments = actionOnOneThing
  , touchableNouns = oneTouchableThing
  , checkRules = makeActionRulebook "check taking rulebook"
    [ cantTakeYourself
    , cantTakeOtherPeople
    , cantTakeComponentParts
    , cantTakePeoplesPossessions
    , cantTakeOutOfPlay
    , cantTakeWhatYoureInside
    , cantTakeWhatsAlreadyTaken
    , cantTakeScenery
    -- we don't need this but... , canOnlyTakeThings
    , cantTakeFixedInPlace
    , usePlayersHoldallIfNeeded
    , cantExceedCarryingCapacity
    ]
  , carryOutRules = makeActionRulebook "carry out switching on rulebook" [ standardTakingRule ]
  , reportRules = makeActionRulebook "report switching on rulebook" [ standardReportTakingRule ]
  }

type TakingRule wm = ActionRule wm (TakingAction wm) (Thing wm)
cantTakeYourself :: TakingRule wm
cantTakeYourself = notImplementedRule "cantTakeYourself rule"

cantTakeOtherPeople :: WithPrintingNameOfSomething wm => TakingRule wm
cantTakeOtherPeople = makeRule "cant take other people rule" [ forKind "person" ] $ \args -> do
  let person = variables args
  [saying|I don't suppose {person} would care for that.|]
  stopTheAction

cantTakeComponentParts :: TakingRule wm
cantTakeComponentParts = notImplementedRule "cantTakeComponentParts rule"

cantTakePeoplesPossessions :: TakingRule wm
cantTakePeoplesPossessions = notImplementedRule "cantTakePeoplesPossessions rule"

cantTakeOutOfPlay :: TakingRule wm
cantTakeOutOfPlay = notImplementedRule "cantTakeOutOfPlay rule"

cantTakeWhatYoureInside :: TakingRule wm
cantTakeWhatYoureInside = notImplementedRule "cantTakeWhatYoureInside rule"

cantTakeWhatsAlreadyTaken :: TakingRule wm
cantTakeWhatsAlreadyTaken = notImplementedRule "cantTakeWhatsAlreadyTaken rule"

cantTakeScenery :: TakingRule wm
cantTakeScenery = notImplementedRule "cantTakeScenery rule"

cantTakeFixedInPlace :: TakingRule wm
cantTakeFixedInPlace = notImplementedRule "cantTakeFixedInPlace rule"

usePlayersHoldallIfNeeded :: TakingRule wm
usePlayersHoldallIfNeeded = notImplementedRule "usePlayersHoldallIfNeeded rule"

cantExceedCarryingCapacity :: TakingRule wm
cantExceedCarryingCapacity = notImplementedRule "cantExceedCarryingCapacity rule"

standardTakingRule :: WMWithProperty wm Enclosing => TakingRule wm
standardTakingRule = makeRule "standard taking rule" [] $ \args -> do
  case getEnclosingMaybe (toAny $ source args) of
    Nothing -> error $ "was not able to take something: " <> (display (source args))
    Just x -> move (variables args) (tagObject @Enclosing @EnclosingTag x (source args))
  rulePass

standardReportTakingRule :: WithPrintingNameOfSomething wm => TakingRule wm
standardReportTakingRule = makeRule "standard report taking rule" [] $ \args -> do
  let vars = variables args
  [saying|Grabbing {the vars}.|]
  rulePass
