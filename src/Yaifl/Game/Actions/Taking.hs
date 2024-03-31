module Yaifl.Game.Actions.Taking where

import Yaifl.Model.Action
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.Kinds.Thing

type TakingAction wm = Action wm () 'TakesThingParameter (Thing wm)
takingAction :: TakingAction wm
takingAction = (makeAction "taking")
  { name = "taking"
  , understandAs = ["take", "get", "grab"]
  , parseArguments = actionOnOneThing
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

cantTakeOtherPeople :: TakingRule wm
cantTakeOtherPeople = notImplementedRule "cantTakeOtherPeople rule"

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

standardTakingRule :: TakingRule wm
standardTakingRule = notImplementedRule "standardTakingRule rule"

standardReportTakingRule :: TakingRule wm
standardReportTakingRule = notImplementedRule "standard report taking rule"
