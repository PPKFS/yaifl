module Yaifl.Zork.World.Inside where

{-

Chapter 2 - The House Interior
Section 1 - Kitchen
Kitchen is a room. Kitchen is in House Interior.
The description of Kitchen is "You are in the kitchen of the white house. A table seems to have been used recently for the preparation of food. A passage leads to the west and a dark staircase can be seen leading upward. A dark chimney leads down and to the east is a small window which is [if the kitchen-window is open]open[otherwise]slightly ajar[end if]."
West of Kitchen is Living Room. Above Kitchen is Attic.
Instead of going down in Kitchen:
  say "Only Santa Claus climbs down chimneys."
The chimney is a backdrop. The chimney is in Kitchen and Studio. Understand "chimney" and "dark" and "narrow" and "fireplace" as the chimney.
The description of the chimney is "[if the player is in Kitchen]The chimney leads downward, and looks climbable.[otherwise]The chimney leads upward, and looks climbable.[end if]"
The kitchen table is a supporter in Kitchen. The kitchen table is scenery.
Understand "table" and "kitchen" as the kitchen table.
The glass bottle is a closed transparent openable container on the kitchen table. "A bottle is sitting on the table."
Understand "bottle" and "container" and "clear" and "glass" as the glass bottle.
The carrying capacity of the glass bottle is 1.
Instead of inserting something into the glass bottle when the glass bottle contains something (called the existing contents):
  say "The bottle is full."
Instead of throwing the glass bottle at something:
  say "The bottle hits the far wall and shatters.";
  if the quantity of water is in the glass bottle:
    remove the quantity of water from play;
  remove the glass bottle from play.
Instead of attacking the glass bottle:
  say "A brilliant maneuver destroys the bottle.";
  if the quantity of water is in the glass bottle:
    remove the quantity of water from play;
  remove the glass bottle from play.
The quantity of water is a thing in the glass bottle.
Understand "water" and "quantity" and "liquid" and "h2o" as the quantity of water.
The description of the quantity of water is "It looks like plain water."
Instead of drinking the quantity of water:
  remove the quantity of water from play;
  say "Thank you very much. I was rather thirsty (from strenuously carrying everything for you)."
Instead of drinking something:
  say "How can you drink that?"
The global-water is a backdrop. The global-water is in Dam-Base, River1, River2, River3, River4, River5, White Cliffs North, White Cliffs South, Sandy Beach, Shore, Aragain Falls, End of Rainbow, Canyon Bottom, On-the-Rainbow, Reservoir-South, Reservoir-North, Stream View, In-Stream, and Reservoir.
The printed name of the global-water is "water".
Understand "water" and "river" and "lake" and "stream" as the global-water.
The description of the global-water is "It looks like water."
Instead of taking the global-water:
  if the player carries the glass bottle:
    if the glass bottle is not open:
      say "The bottle is closed.";
    otherwise if the glass bottle contains something:
      say "The water slips through your fingers.";
    otherwise:
      now the quantity of water is in the glass bottle;
      say "The bottle is now full of water.";
  otherwise:
    say "The water slips through your fingers."
Filling is an action applying to one thing. Understand "fill [something]" as filling.
Carry out filling: say "You can't fill that."
Instead of filling the glass bottle:
  if the player can see the global-water:
    if the glass bottle is not open:
      say "The bottle is closed.";
    otherwise if the glass bottle contains something:
      say "The bottle is full.";
    otherwise:
      now the quantity of water is in the glass bottle;
      say "The bottle is now full of water.";
  otherwise:
    say "There is nothing to fill it with."
Instead of taking the quantity of water when the quantity of water is in the glass bottle:
  say "It's in the bottle. Perhaps you should take that instead."
Instead of dropping the quantity of water:
  if the glass bottle is not open and the quantity of water is in the glass bottle:
    say "The bottle is closed.";
  otherwise if the player is in the magic boat:
    now the quantity of water is in the magic boat;
    say "There is now a puddle in the bottom of the magic boat.";
  otherwise:
    remove the quantity of water from play;
    say "The water spills to the floor and evaporates immediately."
Instead of inserting the quantity of water into something when the second noun is not the glass bottle:
  remove the quantity of water from play;
  say "Nice try."
Instead of throwing the quantity of water at something:
  remove the quantity of water from play;
  say "The water splashes on the walls and evaporates immediately."
Instead of entering the global-water:
  say "You can't swim in the dungeon."
Instead of swimming when the player can see the global-water:
  say "You can't swim in the dungeon."
The brown sack is a closed openable container on the kitchen table. "On the table is an elongated brown sack, smelling of hot peppers."
Understand "bag" and "sack" and "brown" and "elongated" and "smelly" as the brown sack.
The carrying capacity of the brown sack is 2.
Instead of smelling the brown sack:
  if the lunch is in the brown sack:
    say "It smells of hot peppers.";
  otherwise:
    say "It smells faintly of hot peppers."
The lunch is in the brown sack. The description of the lunch is "It looks like a hot pepper sandwich."
Understand "food" and "sandwich" and "lunch" and "dinner" and "hot" and "pepper" as the lunch.
Instead of eating the lunch:
  remove the lunch from play;
  say "Thank you very much. It really hit the spot."
The clove of garlic is in the brown sack. The description of the clove of garlic is "It's a clove of garlic."
Understand "garlic" and "clove" as the clove of garlic.
Instead of eating the clove of garlic:
  remove the clove of garlic from play;
  say "What the heck! You won't make friends this way, but nobody around here is too friendly anyhow. Gulp!"
Section 2 - Attic
Attic is a room. "This is the attic, a low-ceilinged room thick with dust and the faint smell of old wood. Exposed rafters run overhead, and pale light filters through cracks in the boarded-up windows. The only exit is a stairway leading down."
Attic is in House Interior. Attic is a dark room.
The attic table is a supporter in Attic. The attic table is scenery.
Understand "table" as the attic table.
The nasty knife is on the attic table. "On a table is a nasty-looking knife."
Understand "knives" and "knife" and "blade" and "nasty" as the nasty knife.
The rope is in Attic. "A large coil of rope is lying in the corner."
Understand "rope" and "hemp" and "coil" and "large" as the rope.
The description of the rope is "It's a large coil of sturdy hemp rope."
Section 3 - Living Room
Living Room is a room. Living Room is in House Interior.
The description of Living Room is "You are in the living room. There is a doorway to the east[if the magic-flag is true]. To the west is a cyclops-shaped opening in an old wooden door, above which is some strange gothic lettering, [otherwise], a wooden door with strange gothic lettering to the west, which appears to be nailed shut, [end if]a trophy case, [if the rug-moved is false]and a large oriental rug in the center of the room.[otherwise if the trap door is open]and a rug lying beside an open trap door.[otherwise]and a closed trap door at your feet.[end if]"
The trophy case is a transparent openable closed container in Living Room. The trophy case is scenery. "The trophy case is mounted firmly to the wall."
Understand "case" and "trophy" as the trophy case.
The carrying capacity of the trophy case is 100.
Instead of taking the trophy case:
  say "The trophy case is securely fastened to the wall."
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