{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
module Yaifl.Zork.World.Inside where

import Yaifl.Prelude

import Yaifl.Zork.Actions
import Yaifl.AnyObject (asThingOrRoom)
import Effectful.Error.Static
import Yaifl.Action
import Yaifl.Actions.Going
import Yaifl.Actions.Imports
import Yaifl.Backdrop.Create
import Yaifl.Container.Create
import Yaifl.Create.Rule
import Yaifl.Locale (LocaleVariables(..), markAsMentioned)
import Yaifl.Direction.Kind (Direction(..))
import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Query
import Yaifl.Effects.ObjectQuery
import Yaifl.Preconditions
import Yaifl.Person.Query (getPlayerLocation, getPlayer)
import Yaifl.Region.Kind
import Yaifl.Region.Query
import Yaifl.Room.Create
import Yaifl.Room.Query
import Yaifl.Text.DynamicText
import Yaifl.Zork.Specifics
import Yaifl.Effects.Interpreters (WorldConstruction)
import Yaifl.Supporter.Create
import Yaifl.Enclosing.Query
import Yaifl.Container.Query
import Yaifl.Thing.Create
import Yaifl.Container.Kind
import Yaifl.Supporter.Kind
import Yaifl.Enclosing.Kind
import Yaifl.Effects.RuleEffects
import Yaifl.Actions.Inserting
import qualified Data.EnumSet as ES
import Yaifl.Move
import Yaifl.ObjectLike (ThingLike(..))
import Yaifl.Openable.Kind
import Yaifl.Door.Query
import Yaifl.Door.Create
import Yaifl.Combinators
import Yaifl.Vehicle.Create
import qualified Yaifl.Vehicle.Kind as V
import Yaifl.Metadata (getScore', WithMetadata, Score(..), random)
import Yaifl.Thing.Kind
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Zork.ZilVisibility
import Yaifl.Openable.Query (openIt, closeIt)
import qualified Yaifl.Create.CustomActionRule as E

increaseScore :: WithMetadata ZorkWorldModel es => Int -> Eff es ()
increaseScore n = #score %= (\s -> s { currentScore = currentScore s + n })

-- |  Based on the ZIL DUMMY table from the original Zork.
dummyResponse :: WithMetadata ZorkWorldModel es => Eff es Text
dummyResponse = do
  r <- random @Int
  let idx = (r `mod` 3) + 1
  pure $ case idx of
    1 -> "Look around."
    2 -> "Too late for that."
    _ -> "Have your eyes checked."

data InsideTheHouse = InsideTheHouse RoomEntity

theHouseInterior :: RoomEntity -> RegionEntity -> WorldConstruction ZorkWorldModel InsideTheHouse
theHouseInterior kitchen houseInterior = do

  globalBefore checkZilVisibilityGlobal
  globalBefore checkZilVisibilitySecondNoun

  beforeActivity #printingTheLocaleDescription [] "mark zil-invisible things as mentioned" $ \lv -> do
    case lv of
      LocaleVariables _ localeObj _ -> do
        asThingOrRoom
          (const rulePass)
          (\room -> do
            things <- getContents room
            forM_ things $ \thing -> do
              unlessM (isZilVisible thing) $ markAsMentioned thing
            rulePass)
          localeObj
    rulePass

  afterActivity #printingNameOfSomething [] "mark zil-invisible thing as mentioned" $ \obj -> do
    asThingOrRoom
      (\thing -> unlessM (isZilVisible thing) $ markAsMentioned thing)
      (const pass)
      obj
    rulePass

  livingRoom <- addRoom "Living Room" $ newRoom
  attic <- addRoom "Attic" $ newRoom
    & #description .~ "This is the attic, a low-ceilinged room thick with dust and the faint smell of old wood. Exposed rafters run overhead, and pale light filters through cracks in the boarded-up windows. The only exit is a stairway leading down."
    & makeItDark
  studio <- addRoom' "Studio"
  cellar <- addRoom' "Cellar"
  livingRoom `isWestOf` kitchen
  attic `isAbove` kitchen

  insteadOf #going [inDirection Down, whenIn kitchen] $ \_ -> [saying|Only Santa Claus climbs down chimneys.|]
  chimney <- addBackdrop "chimney" $ newBackdrop (InRooms (kitchen:|[studio]))
    & #description .~ text ( do
        inKitchen <- objectEquals kitchen <$> getPlayerLocation
        [sayingTell|{?if inKitchen}The chimney leads downward, and looks climbable.{?else}The chimney leads upward, and looks climbable.{?end if}|]
        )
  chimney `isUnderstoodAs` ["dark", "narrow", "fireplace"]

  kitchenTable <- addSupporter "kitchen table" $ newSupporter
    & makeItScenery
  kitchenTable `isUnderstoodAs` ["table", "kitchen"]

  glassBottle <- addContainer "glass bottle" $ newContainer
    & makeItClosedAndOpenable
    & makeItTransparent
    & #initialAppearance .~ "A bottle is sitting on the table"
    & #carryingCapacity .~ 1

  glassBottle `isUnderstoodAs` ["bottle", "container", "clear", "glass"]

  magicBoat <- addVehicle "magic boat" $ newVehicle
    & #openStatus .~ (Open, Openable)
    & #carryingCapacity .~ 10
  insteadOf' #inserting [intoThe glassBottle, whenContainsSomething glassBottle] $ do
    [saying|The bottle is full.|]
  quantityOfWater <- addThing "quantity of water" $ newThing
    & placeIt (inThe glassBottle)
    & #description .~ "It looks like plain water."
  quantityOfWater `isUnderstoodAs` ["liquid", "h2o"]
  let removeWaterIfInBottleAndBottle :: RuleEffects ZorkWorldModel es => Eff es (Maybe Bool)
      removeWaterIfInBottleAndBottle = do
        c <- getContainer glassBottle
        let waterIn = quantityOfWater `ES.member` (c ^. #enclosing % #contents)
        when waterIn $ removeFromPlay quantityOfWater
        removeFromPlay glassBottle
        pure Nothing
  insteadOf' #throwing [theObject glassBottle] $ do
    [saying|The bottle hits the far wall and shatters.|]
    inject removeWaterIfInBottleAndBottle
  insteadOf' #attacking [theObject glassBottle] $ do
    [saying|A brilliant maneuver destroys the bottle.|]
    inject removeWaterIfInBottleAndBottle
  insteadOf' #drinking [theObject quantityOfWater] $ do
    [saying|Thank you very much. I was rather thirsty (from strenuously carrying everything for you).|]
    removeFromPlay quantityOfWater
  insteadOf' #drinking [] [saying|How can you drink that?|]
  insteadOf' #taking [theObject quantityOfWater, glassBottle `whenInside` quantityOfWater]
    [saying|It's in the bottle. Perhaps you should take that instead.|]
  insteadOf' #dropping [theObject quantityOfWater] $ do
    inBottle <- (quantityOfWater `ES.member`) . (^. #enclosing % #contents) <$> getContainer glassBottle
    bottleClosed <- isClosedContainer <$> getContainer glassBottle
    p <- getPlayer
    playerInBoat <- V.inThe magicBoat `enclosingContains` p
    if
      | inBottle && bottleClosed -> [saying|The bottle is closed.|]
      | playerInBoat -> do
          w <- getThing quantityOfWater
          w `move` magicBoat
          [saying|There is now a puddle in the bottom of the magic boat.|]
      | otherwise -> do
          removeFromPlay quantityOfWater
          [saying|The water spills to the floor and evaporates immediately.|]
  insteadOf' #inserting [theObject quantityOfWater, not_ (intoThe glassBottle)] $ do
    removeFromPlay quantityOfWater
    [saying|Nice try.|]
  insteadOf' #throwing [theObject quantityOfWater] $ do
    removeFromPlay quantityOfWater
    [saying|The water splashes on the walls and evaporates immediately.|]

  brownSack <- addContainer "brown sack" $ newContainer
    & placeIt (onThe kitchenTable)
    & makeItClosedAndOpenable
    & #initialAppearance .~ "On the table is an elongated brown sack, smelling of hot peppers."
    & #carryingCapacity .~ 2
  brownSack `isUnderstoodAs` ["bag", "elongated", "smelly"]

  lunch <- addThing "lunch" $ newThing
    & #description .~ "It looks like a hot pepper sandwich."
    & placeIt (inThe brownSack)

  lunch `isUnderstoodAs` ["food", "sandwich", "dinner", "hot", "pepper"]
  insteadOf' #smelling [theObject brownSack] $ do
    hasLunch <- coerceTag brownSack `enclosingContains` lunch
    if hasLunch then [saying|It smells of hot peppers.|] else [saying|It smells faintly of hot peppers.|]

  insteadOf' #eating [theObject lunch] $ do
    removeFromPlay lunch
    [saying|Thank you very much. It really hit the spot.|]

  cloveOfGarlic <- addThing "clove of garlic" $ newThing
    & #description .~ "It's a clove of garlic."
    & placeIt (inThe brownSack)

  insteadOf' #eating [theObject cloveOfGarlic] $ do
    removeFromPlay cloveOfGarlic
    [saying|What the heck! You won't make friends this way, but nobody around here is too friendly anyhow. Gulp!|]

  [attic, kitchen, livingRoom] `areInRegion` houseInterior

  atticTable <- addSupporter "attic table" $ newSupporter
    & makeItScenery
    & placeIt (inTheRoom attic)

  nastyKnife <- addThing "nasty knife" $ newThing
    & placeIt (onThe atticTable)
    & #initialAppearance .~ "On a table is a nasty-looking knife."

  nastyKnife `isUnderstoodAs` ["knives", "knife", "blade", "nasty"]

  rope <- addThing "rope" $ newThing
    & placeIt (inTheRoom attic)
    & #initialAppearance .~ "A large coil of rope is lying in the corner."
    & #description .~ "It's a large coil of sturdy hemp rope."

  rope `isUnderstoodAs` ["hemp", "coil", "large"]

  trophyCase <- addContainer "trophy case" $ newContainer
    & makeItScenery
    & makeItClosedAndOpenable
    & makeItTransparent
    & placeIt (inTheRoom livingRoom)
    & #initialAppearance .~ "A trophy case is mounted firmly to the wall."
    & #carryingCapacity .~ 100
  insteadOf' #taking [theObject trophyCase] $ do
    [saying|The trophy case is securely fastened to the wall.|]

  ancientMap <- addThing "ancient map" $ newThing
    & placeIt (inThe trophyCase)
    & #description .~ "The map shows a forest with three clearings. The largest clearing contains a house. Three paths leave the large clearing. One of these paths, leading southwest, is marked 'To Stone Barrow'."
    & understandItAs ["parchment", "map", "antique", "old", "ancient"]
  makeZilInvisible ancientMap

  everyTurn "trophy case scoring rule" [] $ do
    newScore <- sum . map (^. #objectData % #thingData % #treasureValue % #unValue) <$> getContents trophyCase
    oldScore <- getValue #trophyCaseScore
    when (newScore /= oldScore) $ do
      let diff = newScore - oldScore
      increaseScore diff
      setValue #trophyCaseScore newScore
    score <- getScore'
    wonFlag <- getValue #wonFlag
    when (score >= 350 && not wonFlag) $ do
      setValue #wonFlag True
      makeZilVisible ancientMap
      [saying|#{linebreak}An almost inaudible voice whispers in your ear, "Look to your treasures for the final secret."#{linebreak}|]

  trapdoor <- addDoor "trap door" $ newDoor (livingRoom, Up) (cellar, Down)
    & makeItScenery
    & makeItClosedAndOpenable
    & understandItAs ["trapdoor", "trap-door", "cover", "trap", "dusty"]
  makeZilInvisible trapdoor

  insteadOf #entering [theObject trapdoor] $ \a -> tryAction "go" [TheDirection Down] a >> pass

  before' #going [inDirection Down, whenIn livingRoom] "trap door check" $ do
    rugMoved <- getValue #rugMovedFlag
    trapDoorIsOpen <- isOpen <$> getDoor trapdoor
    if not rugMoved
      then do
        [saying|You can't go that way.|]
        pure (Just False)
      else if not trapDoorIsOpen
        then do
          [saying|The trap door is closed.|]
          pure (Just False)
        else rulePass

  insteadOf #opening [theObject trapdoor, whenIn livingRoom] $ \_ -> do
    trapDoorIsOpen <- isOpen <$> getDoor trapdoor
    if trapDoorIsOpen
      then do
        resp <- dummyResponse
        [saying|{resp}|]
      else do
        trapdoorThing <- getThing trapdoor
        openIt trapdoorThing
        [saying|The door reluctantly opens to reveal a rickety staircase descending into darkness.|]
        throwError ContinueAction

  insteadOf #closing [theObject trapdoor, whenIn livingRoom] $ \_ -> do
    trapDoorIsOpen <- isOpen <$> getDoor trapdoor
    if not trapDoorIsOpen
      then do
        resp <- dummyResponse
        [saying|{resp}|]
      else do
        trapdoorThing <- getThing trapdoor
        closeIt trapdoorThing
        [saying|The door swings shut and closes.|]
        throwError ContinueAction

  insteadOf' #lookingUnder [theObject trapdoor, whenIn livingRoom] $ do
    trapDoorIsOpen <- isOpen <$> getDoor trapdoor
    if trapDoorIsOpen
      then [saying|You see a rickety staircase descending into darkness.|]
      else [saying|It's closed.|]

  -- Old wooden door
  oldWoodenDoor <- addThing "old wooden door" $ newThing
    & placeIt (inTheRoom livingRoom)
    & makeItScenery
    & #description .~ text (do
        magicFlag <- getValue #magicFlag
        [sayingTell|{?if magicFlag}The door has a cyclops-shaped opening in it.{?else}The engravings translate to "This space intentionally left blank."{?end if}|])
    & understandItAs ["door", "wooden", "gothic", "strange", "lettering", "writing"]

  -- Rules for old wooden door
  insteadOf' #opening [theObject oldWoodenDoor] $ do
    magicFlag <- getValue #magicFlag
    if magicFlag
      then [saying|The door is already open -- the cyclops saw to that.|]
      else [saying|The door is nailed shut.|]

  insteadOf' #going [inDirection West, whenIn livingRoom] $ do
    magicFlag <- getValue #magicFlag
    if magicFlag
      then throwError ContinueAction
      else do
        [saying|The door is nailed shut.|]
        throwError StopAction

  before #going [inDirection West, whenIn livingRoom] "old wooden door check" $ const $ do
    magicFlag <- getValue #magicFlag
    if magicFlag
      then [saying|The door is already open -- the cyclops saw to that.|] >> rulePass
      else do
        [saying|The door is nailed shut.|]
        pure (Just False)

  modifyRoom livingRoom $
    #description .~ text (do
        magicFlag <- getValue #magicFlag
        notRugMoved <- not <$> getValue #rugMovedFlag
        trapDoorOpen <- isOpen <$> getDoor trapdoor
        [sayingTell|You are in the living room. There is a doorway to the east{?if magicFlag}.
To the west is a cyclops-shaped opening in an old wooden door, above which is some strange gothic lettering,
{?else}, a wooden door with strange gothic lettering to the west, which appears to be nailed shut, {?end if}a trophy case,
{?if notRugMoved}and a large oriental rug in the center of the room.{?else if trapDoorOpen}and a rug lying beside an open trap door.
{?else}and a closed trap door at your feet.{?end if}|])

  after #looking [whenIn livingRoom, whenContainsSomething trophyCase] " " $ const $ do
    [saying|Your collection of treasures consists of:|]
    items <- getContents trophyCase
    forM_ items $ \item -> do
      [saying|#{linebreak} {a item}|]
    [saying|#{paragraphBreak}|]
    rulePass

  void $ addThing "sword" $ newThing
    & placeIt (inTheRoom livingRoom)
    & #description .~ "It's an old elvish sword of great antiquity."
    & #initialAppearance .~ "Above the trophy case hangs an elvish sword of great antiquity."
    & #thingModify .~ (#objectData % #thingData % #treasureValue .= 0)
    & understandItAs ["orcrist", "glamdring", "blade", "elvish", "old", "antique"]

  brassLantern <- addThing "brass lantern" $ newThing
    & placeIt (inTheRoom livingRoom)
    & #description .~ text (withThing $ \lantern ->do
        lampBurnedOut <- getValue #lampBurnedOutFlag
        let lanternLit = thingIsLit lantern
        [sayingTell|{?if lampBurnedOut}The lamp has burned out.{?else if lanternLit}The lamp is on.{?else}The lamp is turned off.{?end if}|])
    & #initialAppearance .~ "A battery-powered brass lantern is on the trophy case."
    & understandItAs ["lamp", "lantern", "light", "brass"]

  afterActivity #printingNameOfSomething [theObject' brassLantern] "brass lantern name" $ \lantern -> do
    lampBurnedOut <- getValue #lampBurnedOutFlag
    lanternLit <- thingIsLit <<$>> getThingMaybe lantern
    when (not lampBurnedOut && (not <$?> lanternLit)) [saying| (battery-powered)|]
    rulePass

  insteadOf' #switchingOn [theObject brassLantern] $ do
    lampBurnedOut <- getValue #lampBurnedOutFlag
    if lampBurnedOut
    then [saying|A burned-out lamp won't light.|]
    else do
      modifyThing brassLantern (#objectData % #lit .~ Lit)
      [saying|The brass lantern is now on.|]

  insteadOf' #switchingOff [theObject brassLantern] $ do
    lampBurnedOut <- getValue #lampBurnedOutFlag
    if lampBurnedOut
    then [saying|A burned-out lamp won't light.|]
    else do
      modifyThing brassLantern (#objectData % #lit .~ NotLit)
      [saying|The brass lantern is now off.|]

  insteadOf' #throwing [theObject brassLantern] $ do
    [saying|The lamp has smashed into the floor, and the light has gone out.|]
    modifyThing brassLantern (#objectData % #lit .~ NotLit)
    setValue #lampBurnedOutFlag True
    replaceObject "broken lantern" (newThing
      & #description .~ "The lamp is seriously damaged."
      & understandItAs ["lamp", "lantern", "broken"])
      brassLantern
    rulePass

  carpet <- addThing "carpet" $ newThing
    & placeIt (inTheRoom livingRoom)
    & makeItScenery
    & #description .~ text (do
        rugMoved <- getValue #rugMovedFlag
        if rugMoved
          then [sayingTell|The carpet has been moved to one side of the room.|]
          else [sayingTell|A large oriental rug covers the center of the room.|])
    & understandItAs ["rug", "carpet", "large", "oriental"]

  insteadOf' #taking [theObject carpet] $ do
    [saying|The rug is extremely heavy and cannot be carried.|]

  let doRugMove :: RuleEffects ZorkWorldModel es => Eff es ()
      doRugMove = do
        rugMoved <- getValue #rugMovedFlag
        when rugMoved [saying|Having moved the carpet previously, you find it impossible to move it again.|]
        setValue #rugMovedFlag True
        makeZilVisible trapdoor
        [saying|With a great effort, the rug is moved to one side of the room, revealing the dusty cover of a closed trap door.|]

  insteadOf' #pushing [theObject carpet] doRugMove
  insteadOf' #pulling [theObject carpet] doRugMove

  insteadOf' #entering [theObject carpet] $ do
    rugMoved <- getValue #rugMovedFlag
    trapDoorIsOpen <- isOpen <$> getDoor trapdoor
    if not rugMoved && not trapDoorIsOpen
      then [saying|As you sit, you notice an irregularity underneath it. Rather than be uncomfortable, you stand up again.|]
      else [saying|I suppose you think it's a magic carpet?|]

  insteadOf' #lookingUnder [theObject carpet] $ do
    rugMoved <- getValue #rugMovedFlag
    trapDoorIsOpen <- isOpen <$> getDoor trapdoor
    if not rugMoved && not trapDoorIsOpen
      then [saying|Underneath the rug is a closed trap door. As you drop the corner of the rug, the trap door is once again concealed from view.|]
      else [saying|I suppose you think it's a magic carpet?|]

  E.insteadOf' #raising [theObject carpet] $ do
    rugMoved <- getValue #rugMovedFlag
    if rugMoved
      then [saying|The rug is too heavy to lift.|]
      else [saying|The rug is too heavy to lift, but in trying to take it you have noticed an irregularity beneath it.|]
  pure $ InsideTheHouse livingRoom
