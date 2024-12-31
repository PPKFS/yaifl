module Yaifl.Game.Actions.Taking where

import Yaifl.Model.Action
import Yaifl.Core.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Core.Kinds.Thing
import Yaifl.Text.SayQQ
import Yaifl.Prelude
import Yaifl.Text.Say
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Core.Metadata
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Model.Kinds.Person
import Yaifl.Core.WorldModel
import Yaifl.Core.Actions.GoesWith

type TakingAction wm = Action wm () 'TakesThingParameter (Thing wm)
takingAction :: WMWithProperty wm Person => WMWithProperty wm Enclosing => WithPrintingNameOfSomething wm => TakingAction wm
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

cantTakeScenery :: WMWithProperty wm Person => TakingRule wm
cantTakeScenery = makeRule "can't take scenery" [ Precondition (pure "only scenery") (pure . thingIsScenery . variables)] $ \args -> do
  whenPlayer (source args) $ do
    regarding (Just $ variables args)
    [saying|#{They're} hardly portable.|]
  return (Just False)

cantTakeFixedInPlace :: TakingRule wm
cantTakeFixedInPlace = notImplementedRule "cantTakeFixedInPlace rule"

usePlayersHoldallIfNeeded :: TakingRule wm
usePlayersHoldallIfNeeded = notImplementedRule "usePlayersHoldallIfNeeded rule"

cantExceedCarryingCapacity :: TakingRule wm
cantExceedCarryingCapacity = notImplementedRule "cantExceedCarryingCapacity rule"

standardTakingRule :: WMWithProperty wm Person => WMWithProperty wm Enclosing => TakingRule wm
standardTakingRule = makeRule "standard taking rule" [] $ \args -> do
  case getPersonMaybe (source args) of
    Nothing -> error $ "was not able to take something: " <> (display (source args))
    Just x -> (variables args) `isNowCarriedBy` (tagPersonObject x $ source args)
  rulePass

standardReportTakingRule :: WithPrintingNameOfSomething wm => TakingRule wm
standardReportTakingRule = makeRule "standard report taking rule" [] $ \args -> do
  let vars = variables args
  [saying|Grabbing {the vars}.|]
  rulePass
