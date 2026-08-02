module Yaifl.Zork.World.House where

import Yaifl.Prelude

import Yaifl.Actions.Imports
import Yaifl
import Yaifl.Room.Create
import Yaifl.Text.DynamicText
import Yaifl.Effects.RuleEffects
import Yaifl.Zork.Specifics
import Yaifl.Region.Query (areInRegion)
import Yaifl.Region.Create (addRegion)
import Yaifl.Region.Kind
import Yaifl.Backdrop.Create
import Yaifl.Object.Query
import Yaifl.Create.Rule
import Yaifl.Preconditions
import Yaifl.Thing.Kind
import Yaifl.Object.Kind
import Yaifl.Entity
import Yaifl.Openable.Kind
import Yaifl.ObjectLike
import Yaifl.Door.Create
import Yaifl.Direction.Kind (Direction(..))
import Yaifl.Combinators (makeItScenery, makeItClosedAndOpenable)
import Yaifl.Room.Query
import Yaifl.Actions.Going

data OutsideTheHouse = OutsideTheHouse RoomEntity
roomsOutsideTheHouse :: RegionEntity -> Game ZorkWorldModel OutsideTheHouse
roomsOutsideTheHouse forestArea = do
  houseExterior <- addRegion "House Exterior"
  westOfHouse <- addRoom "West of House" $ newRoom
    & #description .~ (text' $ do
      won <- getValue #wonFlag
      [sayingTell|You are standing in an open field west of a white house, with a boarded front door.{?if won} A secret path leads southwest into the forest.{?end if}|]
      pass)
  whiteHouse <- addBackdrop "white house" $ newBackdrop (InRegions (houseExterior:|[forestArea]))
    & #description .~ "The house is a beautiful colonial house which is painted white. It is clear that the owners must have been extremely wealthy."
  whiteHouse `isUnderstoodAs` ["house", "white", "beautiful", "colonial"]
  let notAtTheHouse :: ArgsMightHaveMainObject v (Thing ZorkWorldModel) => ActionPointer ZorkWorldModel resps goesWith v -> Game ZorkWorldModel ()
      notAtTheHouse l = insteadOf' l [theObject whiteHouse, not_ (whenPlayerIsInRegion houseExterior) ] $ do
        [saying|You're not at the house.|]
  notAtTheHouse #taking
  notAtTheHouse #pushing
  notAtTheHouse #pulling
  notAtTheHouse #touching

  kitchen <- addRoom' "Kitchen"

  behindHouse <- addRoom "Behind House" $ newRoom
  kitchenWindow <- addDoor "kitchen window" $ newDoor
    (kitchen `isToThe` West) (behindHouse `isToThe` East)
    & makeItClosedAndOpenable
    & makeItScenery
  kitchenWindow `isUnderstoodAs` ["window", "kitchen", "small"]

  northOfHouse <- addRoom "North of House" $ newRoom
    & #description .~ "You are facing the north side of a white house. There is no door here, and all the windows are boarded up. To the north a narrow path winds through the trees."
  southOfHouse <- addRoom "South of House" $ newRoom
    & #description .~ "You are facing the south side of a white house. There is no door here, and all the windows are boarded."
  modifyRoom behindHouse $
    #description .~ (text' $ do
        window <- getObject kitchenWindow
        let windowOpen = isOpen window
        [saying|You are behind the white house. A path leads into the forest to the east. In one corner of the house there is a small window which is {?if windowOpen}}open{?else}slightly ajar{?end if}.|]
        )
  [southOfHouse, northOfHouse, behindHouse, westOfHouse] `areInRegion` houseExterior

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

  insteadOf' #entering [theObject whiteHouse, whenPlayerIsInRegion houseExterior, not_ (whenPlayerIsIn behindHouse)] $ do
    [saying|I can't see how to get in from here.|]
  insteadOf' #going [inDirection East, whenPlayerIsIn behindHouse] $ do
    [saying|The door is boarded and you can't remove the boards.|]
  insteadOf #entering [theObject whiteHouse, whenPlayerIsIn behindHouse] $ \a -> do
    window <- getObject kitchenWindow
    if isOpen window
    then tryAction "go" [TheDirection West] a >> pass
    else [saying|The window is closed.|]

  clearing <- addRoom' "Clearing"
  clearing `isEastOf` behindHouse

  boardedWindows <- addBackdrop "boarded window" $ newBackdrop (InRooms (northOfHouse:|[southOfHouse]))
    & #description .~ "The windows are all boarded up."
  boardedWindows `isUnderstoodAs` ["window", "windows", "boarded"]
  insteadOf' #going [inDirection South, whenPlayerIsIn northOfHouse] $ [saying|The windows are all boarded.|]
  insteadOf' #going [inDirection North, whenPlayerIsIn southOfHouse] $ [saying|The windows are all boarded.|]
  return (OutsideTheHouse westOfHouse)
{-
TODO
Finding is an action applying to one visible thing. Understand "find [something]" and "where is [something]" as finding.
Carry out finding: say "I couldn't find that."
Instead of finding the white house when the location of the player is in House Interior:
  say "Why not find your brains?"
Instead of finding the white house when the location of the player is the Clearing:
  say "It seems to be to the west."
Instead of finding the white house when the location of the player is in House Exterior:
  say "It's right here! Are you blind or something?"
Instead of finding the white house when the location of the player is not in House Exterior and the location of the player is not in House Interior and the location of the player is not the Clearing:
  say "It was here just a minute ago...."

Section 3 - Map Connections Around the House

The printed name of the boarded-windows is "boarded window".
Understand "window" and "windows" and "boarded" as the boarded-windows.
Instead of opening the boarded-windows: say "The windows are boarded and can[apostrophe]t be opened."
Instead of attacking the boarded-windows: say "You can[apostrophe]t break the windows open."

Section 7 - Objects Outside the House
The small mailbox is a closed openable container in West-of-House. "There is a small mailbox here."
The description of the small mailbox is "It's a small mailbox."
Understand "mailbox" and "box" as the small mailbox.
The carrying capacity of the small mailbox is 2.
After opening the small mailbox:
  play the sound of creak-sfx as sfx;
  continue the action.
Instead of taking the small mailbox:
  say "It is securely anchored."
The leaflet is in the small mailbox. The description of the leaflet is "WELCOME TO ZORK![paragraph break]ZORK is a game of adventure, danger, and low cunning. In it you will explore some of the most amazing territory ever seen by mortals. No computer should be without one![paragraph break](v4: Modern IF — An Inform 7 translation)[line break]Translated to Inform 7 by John Escobedo[line break]Original by Marc Blank, Dave Lebling, Bruce Daniels, and Tim Anderson[line break]Copyright (c) 1981-1986 Infocom, Inc. ZIL source released under MIT License."
Understand "advertisement" and "leaflet" and "booklet" and "mail" and "small" as the leaflet.
The front door is scenery in West-of-House.
Understand "door" and "front" and "boarded" as the front door.
The description of the front door is "The door is boarded shut."
Instead of opening the front door:
  say "The door cannot be opened."
Instead of attacking the front door:
  say "You can't seem to damage the door."
Instead of burning the front door:
  say "You cannot burn this door."
Instead of looking under the front door:
  say "It won't open."
Instead of reading the front door:
  if the player is in Living Room:
    say "The engravings translate to [quotation mark]This space intentionally left blank.[quotation mark]";
  otherwise:
    say "There is no writing on this side."
The boards are scenery in West-of-House.
Understand "boards" and "board" as the boards.
The description of the boards is "The boards are securely fastened."
Instead of taking the boards:
  say "The boards are securely fastened."
The nails are scenery in West-of-House.
Understand "nails" and "nail" as the nails.
The description of the nails is "The nails are deeply imbedded in the door."
Instead of taking the nails: say "The nails, deeply imbedded in the door, cannot be removed."
Section 8 - Kitchen Window (a door)

The description of the kitchen-window is "[if the kitchen-window-touched is false]The window is slightly ajar, but not enough to allow entry.[otherwise if the kitchen-window is open]The window is open.[otherwise]The window is closed.[end if]".
The kitchen-window-touched is a truth state that varies. The kitchen-window-touched is false.
Instead of opening the kitchen-window:
  if the kitchen-window is open:
    say "It is already open." instead;
  now the kitchen-window is open;
  now the kitchen-window-touched is true;
  play the sound of window-sfx as sfx;
  say "With great effort, you open the window far enough to allow entry."
Instead of closing the kitchen-window:
  if the kitchen-window is not open:
    say "It is already closed." instead;
  now the kitchen-window is not open;
  now the kitchen-window-touched is true;
  say "The window closes (more easily than it opened)."
Instead of searching the kitchen-window:
  if the player is in Kitchen:
    say "You can see a clear area leading towards a forest.";
  otherwise:
    say "You can see what appears to be a kitchen."
-}