module Yaifl.Zork.World.Inside where

import Yaifl.Prelude

import Yaifl.Actions.Going
import Yaifl.Actions.Imports
import Yaifl.Backdrop.Create
import Yaifl.Combinators (makeItScenery, makeItClosedAndOpenable, makeItTransparent, placeIt)
import Yaifl.Container.Create
import Yaifl.Create.Rule
import Yaifl.Direction.Kind (Direction(..))
import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Query
import Yaifl.Person.Query (getPlayerLocation, getPlayer)
import Yaifl.Preconditions
import Yaifl.Region.Kind
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
import Yaifl.Region.Query

removeFromPlay :: a
removeFromPlay = error "todo"

whenInside :: a
whenInside = error "todo"
data InsideTheHouse = InsideTheHouse RoomEntity
theHouseInterior :: RoomEntity -> RegionEntity -> WorldConstruction ZorkWorldModel InsideTheHouse
theHouseInterior kitchen houseInterior = do
  livingRoom <- addRoom "Living Room" $ newRoom
  attic <- addRoom "Attic" $ newRoom
    & #description .~ "This is the attic, a low-ceilinged room thick with dust and the faint smell of old wood. Exposed rafters run overhead, and pale light filters through cracks in the boarded-up windows. The only exit is a stairway leading down."
    & makeItDark
  studio <- addRoom' "Studio"
  livingRoom `isWestOf` kitchen
  attic `isAbove` kitchen

  insteadOf #going [inDirection Down, whenIn kitchen] $ \_ -> [saying|Only Santa Claus climbs down chimneys.|]
  chimney <- addBackdrop "chimney" $ newBackdrop (InRooms (kitchen:|[studio]))
    & #description .~ text' ( do
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

  -- The magic boat is an open enterable vehicle. The carrying capacity of the magic boat is 10.
  magicBoat <- addVehicle "magic boat" $ newVehicle
    & makeItOpenAndEnterable
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
  insteadOf' #taking [theObject quantityOfWater, quantityOfWater `whenInside` glassBottle]
    [saying|It's in the bottle. Perhaps you should take that instead.|]
  insteadOf' #dropping [theObject quantityOfWater] $ do
    inBottle <- (quantityOfWater `ES.member`) . (^. #enclosing % #contents) <$> getContainer glassBottle
    bottleClosed <- isClosedContainer <$> getContainer glassBottle
    p <- getPlayer
    playerInBoat <- getVehicle magicBoat >>= \boat -> boat `enclosingContains` p
    if
      | inBottle && bottleClosed -> [saying|The bottle is closed.|]
      | playerInBoat -> do
          w <- getThing quantityOfWater
          w `move` inThe magicBoat
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

  everyTurn "trophy case scoring rule" [] $ do
    newScore <- sum <$> (traverse (^. #objectData % #thingData % #treasureValue) =<< getContents (coerceTag trophyCase))
    oldScore <- getValue #trophyCaseScore
    when (newScore /= oldScore) $ do
      let diff = newScore - oldScore
      increaseScore diff
      setValue #trophyCaseScore newScore
    score <- getScore
    wonFlag <- getValue #wonFlag
    when (score >= 350 && not wonFlag) $ do
      setValue #wonFlag True
      makeAncientMapZilVisible
      [saying|[line break]An almost inaudible voice whispers in your ear, [quotation mark]Look to your treasures for the final secret.[quotation mark][line break]|]

  return (error "")


{-
#description .~ text' ( do
        magicFlag <- getMagicFlag
        notRugMoved <- getRugMoved
        trapDoorOpen <- isOpenDoor <$> getDoor trapdoor
        [sayingTell|You are in the living room. There is a doorway to the east{?if magicFlag}.
To the west is a cyclops-shaped opening in an old wooden door, above which is some strange gothic lettering,
{?else}, a wooden door with strange gothic lettering to the west, which appears to be nailed shut, {?end if}a trophy case,
{?if notRugMoved}and a large oriental rug in the center of the room.{?else if trapDoorOpen}and a rug lying beside an open trap door.
{?else}and a closed trap door at your feet.{?end if}|]
        )}

Chapter 7 - Trophy Case Scoring
Every turn (this is the trophy case scoring rule):
  let new-score be 0;
  repeat with item running through things in the trophy case:
    increase new-score by the treasure-value of the item;
    repeat with inner running through things enclosed by the item:
      increase new-score by the treasure-value of the inner;
  if new-score is not the trophy-case-score:
    let diff be new-score minus the trophy-case-score;
    increase the score by diff;
    now the trophy-case-score is new-score;
  if the score is at least 350 and the won-flag is false:
    now the won-flag is true;
    now the ancient map is zil-visible;
    say "[line break]An almost inaudible voice whispers in your ear, [quotation mark]Look to your treasures for the final secret.[quotation mark][line break]".
After looking when the location is Living Room and the number of things in the trophy case is greater than 0:
  say "Your collection of treasures consists of:";
  repeat with item running through things in the trophy case:
    say "[line break]  [a item]";
  say "[paragraph break]".
The sword is in Living Room. "Above the trophy case hangs an elvish sword of great antiquity."
Understand "sword" and "orcrist" and "glamdring" and "blade" and "elvish" and "old" and "antique" as the sword.
The description of the sword is "It's an old elvish sword of great antiquity."
The treasure-value of the sword is 0.
The brass lantern is in Living Room. "A battery-powered brass lantern is on the trophy case."
Understand "lamp" and "lantern" and "light" and "brass" as the brass lantern.
After printing the name of the brass lantern:
  if the lamp-burned-out is false and the brass lantern is not lit:
    say " (battery-powered)".
The description of the brass lantern is "[if the lamp-burned-out is true]The lamp has burned out.[otherwise if the brass lantern is lit]The lamp is on.[otherwise]The lamp is turned off.[end if]".
Instead of switching on the brass lantern:
  if the lamp-burned-out is true:
    say "A burned-out lamp won't light." instead;
  now the brass lantern is lit;
  say "The brass lantern is now on."
Instead of switching off the brass lantern:
  if the lamp-burned-out is true:
    say "The lamp has already burned out." instead;
  now the brass lantern is not lit;
  say "The brass lantern is now off."
The broken lamp is a thing. The printed name of the broken lamp is "broken lantern".
Understand "lamp" and "lantern" and "broken" as the broken lamp.
The description of the broken lamp is "The lamp is seriously damaged."
Instead of switching on the broken lamp: say "The lamp is broken."
Instead of switching off the broken lamp: say "The lamp is broken."
Instead of throwing the brass lantern at something:
  say "The lamp has smashed into the floor, and the light has gone out.";
  now the brass lantern is not lit;
  now the lamp-burned-out is true;
  now the broken lamp is in the location;
  remove the brass lantern from play.
The old wooden door is scenery in Living Room. Understand "door" and "wooden" and "gothic" and "strange" and "lettering" and "writing" as the old wooden door.
The description of the old wooden door is "[if the magic-flag is true]The door has a cyclops-shaped opening in it.[otherwise]The engravings translate to 'This space intentionally left blank.'[end if]".
Instead of opening the old wooden door:
  if the magic-flag is true:
    say "The door is already open -- the cyclops saw to that.";
  otherwise:
    say "The door is nailed shut."
Instead of going west in Living Room:
  if the magic-flag is true:
    continue the action;
  say "The door is nailed shut."
Section 4 - Rug and Trap Door Puzzle
The rug-moved is a truth state that varies. The rug-moved is false.
The carpet is scenery in Living Room. Understand "rug" and "carpet" and "large" and "oriental" as the carpet.
The description of the carpet is "[if the rug-moved is false]A large oriental rug covers the center of the room.[otherwise]The carpet has been moved to one side of the room.[end if]".
Instead of taking the carpet:
  say "The rug is extremely heavy and cannot be carried."
Instead of pushing the carpet:
  try the-rug-move.
Instead of pulling the carpet:
  try the-rug-move.
The-rug-move is an action applying to nothing.
Carry out the-rug-move:
  if the rug-moved is true:
    say "Having moved the carpet previously, you find it impossible to move it again." instead;
  now the rug-moved is true;
  now the trap door is zil-visible;
  say "With a great effort, the rug is moved to one side of the room, revealing the dusty cover of a closed trap door."
Instead of entering the carpet:
  if the rug-moved is false and the trap door is not open:
    say "As you sit, you notice an irregularity underneath it. Rather than be uncomfortable, you stand up again.";
  otherwise:
    say "I suppose you think it[apostrophe]s a magic carpet?"
Instead of looking under the carpet:
  if the rug-moved is false and the trap door is not open:
    say "Underneath the rug is a closed trap door. As you drop the corner of the rug, the trap door is once again concealed from view.";
  otherwise:
    say "I suppose you think it's a magic carpet?"
Instead of raising the carpet:
  if the rug-moved is true:
    say "The rug is too heavy to lift.";
  otherwise:
    say "The rug is too heavy to lift, but in trying to take it you have noticed an irregularity beneath it."
The trap door is a door. The trap door is scenery. The trap door is closed and openable.
Understand "door" and "trapdoor" and "trap-door" and "cover" and "trap" and "dusty" as the trap door.
The trap door is below Living Room and above Cellar.
A thing can be zil-visible or zil-invisible. A thing is usually zil-visible. The trap door is zil-invisible.
Rule for writing a paragraph about a zil-invisible thing: now the item described is mentioned.
Before printing the locale description of a room (called the place):
  repeat with item running through zil-invisible things in the place:
    now item is mentioned.
Before doing anything to a zil-invisible thing:
  say "You can't see any such thing." instead.
Before doing anything when the second noun is a zil-invisible thing:
  say "You can't see any such thing." instead.
Instead of entering the trap door: try going down.
Before going down in Living Room:
  if the rug-moved is false:
    say "You can't go that way." instead;
  if the trap door is not open:
    say "The trap door is closed." instead.
Instead of opening the trap door when the player is in Living Room:
  if the trap door is open:
    say "[dummy]" instead;
  now the trap door is open;
  say "The door reluctantly opens to reveal a rickety staircase descending into darkness."
Instead of closing the trap door when the player is in Living Room:
  if the trap door is not open:
    say "It is already closed." instead;
  now the trap door is not open;
  say "The door swings shut and closes."
Instead of looking under the trap door when the player is in Living Room:
  if the trap door is open:
    say "You see a rickety staircase descending into darkness.";
  otherwise:
    say "It[apostrophe]s closed."
The trap-door-touched is a truth state that varies. The trap-door-touched is false.
-}