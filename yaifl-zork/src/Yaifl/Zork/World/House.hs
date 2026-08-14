module Yaifl.Zork.World.House where

import Yaifl.Prelude

import Yaifl.Actions.Going
import Yaifl.Actions.Imports
import Yaifl.Backdrop.Create
import Yaifl.Combinators (makeItScenery, makeItClosedAndOpenable, placeIt, makeItPlural)
import Yaifl.Container.Create
import Yaifl.Container.Kind
import Yaifl.Create.Rule
import Yaifl.Direction.Kind (Direction(..))
import Yaifl.Door.Create
import Yaifl.Effects.RuleEffects
import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Query
import Yaifl.ObjectLike
import Yaifl.Openable.Kind
import Yaifl.Openable.Query (openIt, closeIt)
import Yaifl.Person.Query (getPlayerLocation)
import Yaifl.Preconditions
import Yaifl.Property.Has
import Yaifl.Region.Create (addRegion)
import Yaifl.Region.Kind
import Yaifl.Region.Query (areInRegion, isInRegion)
import Yaifl.Room.Create
import Yaifl.Room.Query
import Yaifl.Tag
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.DynamicText
import Yaifl.Thing.Create
import Yaifl.Thing.Kind
import Yaifl.Zork.Specifics
import qualified Yaifl.Create.CustomActionRule as E
import Yaifl.Effects.Interpreters (WorldConstruction)
import Yaifl.Zork.Actions

containerModify :: forall wm. (WMWithProperty wm Container) => (Container -> Container) -> Eff '[State (Thing wm)] ()
containerModify f = #specifics % propertyAT %= f

data OutsideTheHouse = OutsideTheHouse RoomEntity
roomsOutsideTheHouse :: RegionEntity -> WorldConstruction ZorkWorldModel OutsideTheHouse
roomsOutsideTheHouse forestArea = do

  -- make the rooms
  houseExterior <- addRegion "House Exterior"
  houseInterior <- addRegion "House Interior"
  westOfHouse <- addRoom "West of House" $ newRoom
    & #description .~ (text' $ do
      won <- getValue #wonFlag
      [sayingTell|You are standing in an open field west of a white house, with a boarded front door.{?if won} A secret path leads southwest into the forest.{?end if}|]
      pass)
  -- forward declare a couple of nearby rooms because we need to refer to them
  kitchen <- addRoom' "Kitchen"
  clearing <- addRoom "Clearing" $ newRoom
    & #description .~ "You are in a small clearing in a well marked forest path that extends to the east and west."
  clearing `isInRegion` forestArea
  livingRoom <- addRoom' "Living Room"

  behindHouse <- addRoom "Behind House" $ newRoom

  northOfHouse <- addRoom "North of House" $ newRoom
    & #description .~ "You are facing the north side of a white house. There is no door here, and all the windows are boarded up. To the north a narrow path winds through the trees."
  southOfHouse <- addRoom "South of House" $ newRoom
    & #description .~ "You are facing the south side of a white house. There is no door here, and all the windows are boarded."

  [southOfHouse, northOfHouse, behindHouse, westOfHouse] `areInRegion` houseExterior

  -- position the rooms
  clearing `isEastOf` behindHouse
  northOfHouse `isNorthOf` westOfHouse
  southOfHouse `isSouthOf` westOfHouse
  northOfHouse `isNorthEastOf` westOfHouse
  southOfHouse `isSouthEastOf` westOfHouse
  northOfHouse `isNorthOf` behindHouse
  southOfHouse `isSouthOf` behindHouse
  southOfHouse `isSouthWestOf` behindHouse
  northOfHouse `isNorthWestOf` behindHouse
  behindHouse `isEastOf` southOfHouse
  westOfHouse `isWestOf` southOfHouse
  behindHouse `isNorthEastOf` southOfHouse
  westOfHouse `isNorthWestOf` southOfHouse
  behindHouse `isEastOf` northOfHouse
  westOfHouse `isWestOf` northOfHouse

  -- add the house scenery
  whiteHouse <- addBackdrop "white house" $ newBackdrop (InRegions (houseExterior:|[forestArea]))
    & #description .~ "The house is a beautiful colonial house which is painted white. It is clear that the owners must have been extremely wealthy."
  whiteHouse `isUnderstoodAs` ["house", "white", "beautiful", "colonial"]
  let notAtTheHouse :: ArgsMightHaveMainObject v (Thing ZorkWorldModel) => ActionPointer ZorkWorldModel resps goesWith v -> WorldConstruction ZorkWorldModel ()
      notAtTheHouse l = insteadOf' l [theObject whiteHouse, not_ (whenPlayerIsInRegion houseExterior) ] $ do
        [saying|You're not at the house.|]
  notAtTheHouse #taking
  notAtTheHouse #pushing
  notAtTheHouse #pulling
  notAtTheHouse #touching

  insteadOf' #entering [theObject whiteHouse, whenPlayerIsInRegion houseExterior, not_ (whenPlayerIsIn behindHouse)] $ do
    [saying|I can't see how to get in from here.|]
  insteadOf' #going [inDirection East, whenPlayerIsIn westOfHouse] $ do
    [saying|The door is boarded and you can't remove the boards.|]

  -- add some objects
  boardedWindows <- addBackdrop "boarded window" $ newBackdrop (InRooms (northOfHouse:|[southOfHouse]))
    & #description .~ "The windows are all boarded up."
  boardedWindows `isUnderstoodAs` ["window", "windows", "boarded"]
  insteadOf' #going [inDirection South, whenPlayerIsIn northOfHouse] $ [saying|The windows are all boarded.|]
  insteadOf' #going [inDirection North, whenPlayerIsIn southOfHouse] $ [saying|The windows are all boarded.|]

  insteadOf' #opening [theObject boardedWindows] $ [saying|The windows are boarded and can't be opened.|]
  insteadOf' #attacking [theObject boardedWindows] $ [saying|You can't break the windows open.|]

  mailbox <- addContainer "small mailbox" $ newContainer
    & makeItClosedAndOpenable
    & #initialAppearance .~ "There is a small mailbox here."
    & #description .~ "It's a small mailbox."
    & #location ?~ coerceTag westOfHouse
    & #thingModify .~ (do
        containerModify $ #enclosing % #capacity ?~ 2
      )
  {-
  After opening the small mailbox:
  play the sound of creak-sfx as sfx;
  continue the action.
  -}
  mailbox `isUnderstoodAs` ["mailbox", "box"]
  insteadOf' #taking [theObject mailbox] $ [saying|It is securely anchored.|]

  leaflet <- addThing "leaflet" $ newThing
    & #description .~ (text' [sayingTell|WELCOME TO ZORK!#{paragraphBreak}ZORK is a game of adventure, danger, and low cunning.
In it you will explore some of the most amazing territory ever seen by mortals. No computer should be without one!#{paragraphBreak}
Translated to Yaifl by PPK, based on the Inform 7 translation by John Escobedo.#{linebreak}Original by Marc Blank, Dave Lebling, Bruce Daniels, and Tim Anderson.
#{linebreak}Copyright (c) 1981-1986 Infocom, Inc. ZIL source released under MIT License.|])
    & placeIt (inThe mailbox)
  leaflet `isUnderstoodAs` ["advertisement", "leaflet", "mail", "small"]

  frontDoor <- addThing "front door" $ newThing
    & #description .~ "The door is boarded shut."
    & makeItScenery
    & #location .~ (Just . coerceTag $ westOfHouse)
  frontDoor `isUnderstoodAs` ["door", "front", "boarded"]

  insteadOf' #opening [theObject frontDoor] $ [saying|The door cannot be opened.|]
  insteadOf' #attacking [theObject frontDoor] $ [saying|You can't seem to damage the door.|]
  insteadOf' #burning [theObject frontDoor] $ [saying|You cannot burn this door.|]
  insteadOf' #lookingUnder [theObject frontDoor] $ [saying|It won't open.|]

  boards <- addThing "boards" $ newThing
    & makeItScenery
    & #description .~ "The boards are securely fastened."
    & makeItPlural
    & #location .~ (Just . coerceTag $ westOfHouse)
  insteadOf' #taking [theObject boards] $ [saying|The boards are securely fastened.|]
  boards `isUnderstoodAs` ["board"]

  nails <- addThing "nails" $ newThing
    & makeItScenery
    & #description .~ "The nails are deeply imbedded in the door."
    & makeItPlural
    & #location .~ (Just . coerceTag $ westOfHouse)
  insteadOf' #taking [theObject nails] $ [saying|The nails, deeply imbedded in the door, cannot be removed.|]
  nails `isUnderstoodAs` ["nail"]

  -- kitchen window
  kitchenWindow <- addDoor "kitchen window" $ newDoor
    (kitchen `isToThe` West) (behindHouse `isToThe` East)
    & makeItClosedAndOpenable
    & makeItScenery
    & #description .~ (text' $ withThing $ \t -> do
      notTouched <- not <$> getValue #kitchenWindowTouched
      let windowOpen = isOpen t
      [sayingTell|{?if notTouched}The window is slightly ajar, but not enough to allow entry.{?else if windowOpen}The window is open.{?else}The window is closed.{?end if}|]
      )
  kitchenWindow `isUnderstoodAs` ["window", "kitchen", "small"]
  modifyRoom behindHouse $
    #description .~ (text' $ do
        window <- getObject kitchenWindow
        let windowOpen = isOpen window
        [saying|You are behind the white house. A path leads into the forest to the east. In one corner of the house there is a small window which is {?if windowOpen}open{?else}slightly ajar{?end if}.|]
        )
  insteadOf #entering [theObject whiteHouse, whenPlayerIsIn behindHouse] $ \a -> do
    window <- getObject kitchenWindow
    if isOpen window
    then tryAction "go" [TheDirection West] a >> pass
    else [saying|The window is closed.|]

  insteadOf #opening [theObject kitchenWindow] $ \a -> do
    let w = variables a
    if isOpen w
      then [saying|It is already open.|]
      else do
        openIt w
        setValue #kitchenWindowTouched True
        -- play the sound of window-sfx as sfx;
        [saying|With great effort, you open the window far enough to allow entry.|]
  insteadOf #closing [theObject kitchenWindow] $ \a -> do
    let w = variables a
    if isClosed w
      then [saying|It is already closed.|]
      else do
        closeIt w
        setValue #kitchenWindowTouched True
        -- play the sound of window-sfx as sfx;
        [saying|The window closes (more easily than it opened).|]
  insteadOf' #searching [theObject kitchenWindow] $ do
    p <- getPlayerLocation
    if p `objectEquals` kitchen then [saying|You can see a clear area leading towards a forest.|]
    else [saying|You can see what appears to be a kitchen.|]

  modifyRoom kitchen $
    #description .~ (text' $ do
        window <- getObject kitchenWindow
        let windowOpen = isOpen window
        [sayingTell|You are in the kitchen of the white house. A table seems to have been used recently for the preparation of food. A passage leads to the west and a dark staircase can be seen leading upward. A dark chimney leads down and to the east is a small window which is {?if windowOpen}open{?else}slightly ajar{?end if}.|])

  -- note the translation considers it possible to read this door from inside, which is a different door.
  -- it also considers reading to be examining..
  E.insteadOf' #reading [theObject frontDoor] $ [saying|There is no writing on this side.|]
  -- you also cannot see the white house from inside the house, so you cannot get the "find your brains" message.
  E.insteadOf' #finding [theObject whiteHouse, whenPlayerIsIn clearing] $ [saying|It seems to be to the west.|]
  E.insteadOf' #finding [theObject whiteHouse, whenPlayerIsInRegion houseExterior] $ [saying|It's right here! Are you blind or something?|]
  E.insteadOf' #finding
    [ theObject whiteHouse
    , not_ (whenPlayerIsInRegion houseExterior)
    , not_ (whenPlayerIsInRegion houseInterior)
    , not_ (whenPlayerIsIn clearing) ] $
      [saying|It was here just a minute ago....|]
  return (OutsideTheHouse westOfHouse)

-- Test commands for all logic in this module
testMeWith :: [Text]
testMeWith =
  [
    "look"
  , "examine house"
  , "examine door"
  , "x boards"
  , "x nails"
  , "examine mailbox"

  , "open mailbox"
  , "take leaflet"
  , "examine leaflet"
  , "take mailbox"
  , "open door"
  , "attack door"
  , "burn door"
  , "look under door"
  , "take boards"
  , "take nails"

  , "enter house" -- this prints nothing
  , "go east"
  , "go north"
  , "look"
  , "examine windows"
  , "south"
  , "open windows"
  , "attack windows"
  , "enter house" -- this prints nothing

  , "e"
  , "s"
  , "look"
  , "go north"
  , "enter house"

  , "go east"
  , "look"
  , "go east"
  , "take house"
  , "push house"
  , "pull house"
  , "touch house"
  , "go west"
  , "examine window"

  , "open window"
  , "examine window"
  , "search window"
  , "close window"
  , "examine window"
  , "close window"
-- 46
  , "search window"
  , "open window"
  , "find house"
  , "go east"
  , "find house" -- it seems to be to the west
  , "go west"
  , "go north"
  , "find house"
  , "go west"
  , "find house"
  , "go south"
  , "find house"
  , "go west"
  , "read door" -- need to have actual stuff
  ]
