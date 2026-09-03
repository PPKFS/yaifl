module Yaifl.Zork.World.Mine where

{- TODO: Chapter 19 - Coal Mine Area -}

{-
Mine Entrance is a dark room. "You are standing at the entrance of what might have been a coal mine. The shaft enters the west wall, and there is another exit on the south end of the room."
Mine Entrance is in the Underground.
-}

{-
South of Mine Entrance is Slide Room.
-}

{-
Squeaky Room is a dark room. "You are in a small room. Strange squeaky sounds may be heard coming from the passage at the north end. You may also escape to the east."
Squeaky Room is in the Underground.
-}

{-
West of Mine Entrance is Squeaky Room. East of Squeaky Room is Mine Entrance. North of Squeaky Room is Bat-Room.
-}

{-
Bat-Room is a dark room. Bat-Room is in the Underground.
The printed name of Bat-Room is "Bat Room".
-}

{-
South of Bat-Room is Squeaky Room. East of Bat-Room is Shaft Room.
-}

{-
The bat is a person in Bat-Room. The bat is scenery.
Understand "bat" and "vampire" and "deranged" as the bat.
-}

{-
The description of the bat is "[if the player encloses the clove of garlic]You can see a deranged vampire bat cowering in the corner, repelled by the garlic.[otherwise]A deranged vampire bat is swooping overhead.[end if]".
-}

{-
Instead of taking or attacking the bat:
  if the player encloses the clove of garlic or the clove of garlic is in Bat-Room:
    say "You can't reach him; he's on the ceiling.";
  otherwise:
    say "    Fweep![line break]    Fweep![line break]    Fweep![line break]".
-}

{-
Instead of telling the bat about something:
  say "    Fweep![line break]    Fweep![line break]    Fweep![line break]    Fweep![line break]    Fweep![line break]    Fweep!"
-}

{-
Instead of going north in Bat-Room:
  if the player carries the clove of garlic or the clove of garlic is in Bat-Room:
    continue the action;
  otherwise:
    play the sound of bat-sfx as sfx;
    say "    Fweep![line break]    Fweep![line break]    Fweep![line break][line break]The bat grabs you by the scruff of your neck and lifts you away....[paragraph break]";
    let R be a random number between 1 and 8;
    if R is 1:
      move the player to Mine1;
    otherwise if R is 2:
      move the player to Mine2;
    otherwise if R is 3:
      move the player to Mine3;
    otherwise if R is 4:
      move the player to Mine4;
    otherwise if R is 5:
      move the player to Ladder Top;
    otherwise if R is 6:
      move the player to Ladder Bottom;
    otherwise if R is 7:
      move the player to Squeaky Room;
    otherwise:
      move the player to Mine Entrance.
-}

{-
The description of Bat-Room is "You are in a small room which has doors only to the east and south. [if the clove of garlic is enclosed by the player]In the corner of the room on the ceiling is a large vampire bat who is obviously deranged and holding his nose.[otherwise]A large vampire bat, hanging from the ceiling, swoops down at you![end if]".
-}

{-
The jade figurine is in Bat-Room. "There is an exquisite jade figurine here."
The printed name of the jade figurine is "exquisite jade figurine".
Understand "figurine" and "jade" and "exquisite" as the jade figurine.
The treasure-value of the jade figurine is 5.
The point-value of the jade figurine is 5.
-}

{-
Shaft Room is a dark room. "This is a large room, in the middle of which is a small shaft descending through the floor into darkness below. To the west and the north are exits from this room. Constructed over the top of the shaft is a metal framework to which a heavy iron chain is attached."
Shaft Room is in the Underground.
-}

{-
West of Shaft Room is Bat-Room. North of Shaft Room is Smelly Room.
-}

{-
Instead of going down in Shaft Room:
  say "You wouldn't fit and would die if you could."
-}

{-
Smelly Room is a dark room. "This is a small nondescript room. However, from the direction of a small descending staircase a foul odor can be detected. To the south is a narrow tunnel."
Smelly Room is in the Underground.
-}

{-
Down from Smelly Room is Gas Room. South of Smelly Room is Shaft Room.
-}

{-
Gas Room is a dark room. "This is a small room which smells strongly of coal gas. There is a short climb up some stairs and a narrow tunnel leading east."
Gas Room is in the Underground.
-}

{-
Up from Gas Room is Smelly Room. East of Gas Room is Mine1.
-}

{-
The gas-pseudo is a backdrop. The gas-pseudo is in Gas Room and Smelly Room.
The printed name of the gas-pseudo is "gas".
Understand "gas" and "coal gas" and "odor" and "foul" and "smell" as the gas-pseudo.
The description of the gas-pseudo is "It smells like coal gas in here."
-}

{-
Blowing is an action applying to one thing. Understand "blow [something]" and "blow out [something]" as blowing.
Carry out blowing: say "You can't blow that."
-}

{-
Instead of blowing the gas-pseudo:
  say "There is too much gas to blow away."
-}

{-
Instead of smelling the gas-pseudo:
  say "It smells like coal gas in here."
-}

{-
Instead of switching on the torch when the player is in Gas Room:
  die saying "How sad for an aspiring adventurer to light a torch in a room which reeks of gas. Fortunately, there is justice in the world.[paragraph break]   ** BOOOOOOOOOOOM **"
-}

{-
Instead of burning the pair of candles when the player is in Gas Room:
  die saying "How sad for an aspiring adventurer to light candles in a room which reeks of gas. Fortunately, there is justice in the world.[paragraph break]   ** BOOOOOOOOOOOM **"
-}

{-
Instead of burning the matchbook when the player is in Gas Room:
  die saying "How sad for an aspiring adventurer to light a match in a room which reeks of gas. Fortunately, there is justice in the world.[paragraph break]   ** BOOOOOOOOOOOM **"
-}

{-
Every turn when the player is in Gas Room (this is the gas room explosion rule):
  if the torch is lit and the player carries the torch:
    die saying "Oh dear. It appears that the smell coming from this room was coal gas. I would have thought twice about carrying flaming objects in here.[paragraph break]   ** BOOOOOOOOOOOM **";
  if the pair of candles is lit and the player carries the pair of candles:
    die saying "Oh dear. It appears that the smell coming from this room was coal gas. I would have thought twice about carrying flaming objects in here.[paragraph break]   ** BOOOOOOOOOOOM **";
  if the match-lit is true:
    die saying "Oh dear. It appears that the smell coming from this room was coal gas. I would have thought twice about carrying flaming objects in here.[paragraph break]   ** BOOOOOOOOOOOM **".
-}

{-
The sapphire-encrusted bracelet is in Gas Room. "There is a sapphire-encrusted bracelet here."
Understand "bracelet" and "jewel" and "sapphire" as the sapphire-encrusted bracelet.
The treasure-value of the sapphire-encrusted bracelet is 5.
The point-value of the sapphire-encrusted bracelet is 5.
-}

{-
Mine1 is a dark room. The printed name of Mine1 is "Coal Mine". "This is a nondescript part of a coal mine."
Mine1 is in the Underground.
-}

{-
North of Mine1 is Gas Room. East of Mine1 is Mine1. Northeast of Mine1 is Mine2.
-}

{-
Mine2 is a dark room. The printed name of Mine2 is "Coal Mine". "This is a nondescript part of a coal mine."
Mine2 is in the Underground.
-}

{-
North of Mine2 is Mine2. South of Mine2 is Mine1. Southeast of Mine2 is Mine3.
-}

{-
Mine3 is a dark room. The printed name of Mine3 is "Coal Mine". "This is a nondescript part of a coal mine."
Mine3 is in the Underground.
-}

{-
South of Mine3 is Mine3. Southwest of Mine3 is Mine4. East of Mine3 is Mine2.
-}

{-
Mine4 is a dark room. The printed name of Mine4 is "Coal Mine". "This is a nondescript part of a coal mine."
Mine4 is in the Underground.
-}

{-
North of Mine4 is Mine3. West of Mine4 is Mine4. Down from Mine4 is Ladder Top.
-}

{-
Ladder Top is a dark room. "This is a very small room. In the corner is a rickety wooden ladder, leading downward. It might be safe to descend. There is also a staircase leading upward."
Ladder Top is in the Underground.
-}

{-
Down from Ladder Top is Ladder Bottom. Up from Ladder Top is Mine4.
-}

{-
Ladder Bottom is a dark room. "This is a rather wide room. On one side is the bottom of a narrow wooden ladder. To the west and the south are passages leaving the room."
Ladder Bottom is in the Underground.
-}

{-
South of Ladder Bottom is Dead End 5. West of Ladder Bottom is Timber Room. Up from Ladder Bottom is Ladder Top.
-}

{-
Dead End 5 is a dark room. The printed name of Dead End 5 is "Dead End". "You have come to a dead end in the mine."
Dead End 5 is in the Underground.
North of Dead End 5 is Ladder Bottom.
-}

{-
The small pile of coal is in Dead End 5. "There is a small pile of coal here."
Understand "coal" and "pile" and "heap" and "small" as the small pile of coal.
The description of the small pile of coal is "It's a small pile of coal."
-}

{-
Timber Room is a dark room. "This is a long and narrow passage, which is cluttered with broken timbers. A wide passage comes from the east and turns at the west end of the room into a very narrow passageway. From the west comes a strong draft."
Timber Room is in the Underground.
East of Timber Room is Ladder Bottom.
-}

{-
Instead of going west in Timber Room when the player-is-dead is true:
  say "You cannot enter in your condition."
-}

{-
Instead of going west in Timber Room:
  let heavy-found be false;
  repeat with item running through things carried by the player:
    unless the item is the clove of garlic or the item is the matchbook:
      now heavy-found is true;
  if heavy-found is true:
    say "You cannot fit through this passage with that load.";
  otherwise:
    move the player to Drafty Room.
-}

{-
Drafty Room is a dark room. The printed name of Drafty Room is "Drafty Room". "This is a small drafty room in which is the bottom of a long shaft. To the south is a passageway and to the east a very narrow passage. In the shaft can be seen a heavy iron chain."
Drafty Room is in the Underground.
South of Drafty Room is Machine-Room.
-}

{-
Instead of going east in Drafty Room:
  let heavy-found be false;
  repeat with item running through things carried by the player:
    unless the item is the clove of garlic or the item is the matchbook:
      now heavy-found is true;
  if heavy-found is true:
    say "You cannot fit through this passage with that load.";
  otherwise:
    move the player to Timber Room.
-}

{-
The chain-pseudo is a backdrop. The chain-pseudo is in Shaft Room and Drafty Room.
The printed name of the chain-pseudo is "chain".
Understand "chain" and "iron" and "heavy" as the chain-pseudo.
The description of the chain-pseudo is "The chain secures a basket within the shaft."
-}

{-
Instead of taking or pushing or pulling the chain-pseudo:
  say "The chain is secure."
-}

{-
Instead of raising the chain-pseudo:
  say "Perhaps you should do that to the basket."
-}

{-
Instead of lowering the chain-pseudo:
  say "Perhaps you should do that to the basket."
-}

{-
The lowered-basket is in Drafty Room. The printed name of the lowered-basket is "basket". "From the chain is suspended a basket."
Understand "cage" and "dumbwaiter" and "basket" as the lowered-basket.
-}

{-
The raised-basket is an open container in Shaft Room. The printed name of the raised-basket is "basket". "At the end of the chain is a basket."
Understand "cage" and "dumbwaiter" and "basket" as the raised-basket.
The carrying capacity of the raised-basket is 10.
-}

{-
Instead of taking the raised-basket: say "The cage is securely fastened to the iron chain."
Instead of taking the lowered-basket: say "The cage is securely fastened to the iron chain."
-}

{-
The basket-is-at-top is a truth state that varies. The basket-is-at-top is true.
-}

{-
Raising is an action applying to one thing. Understand "raise [something]" as raising.
Carry out raising: say "You can't raise that."
-}

{-
Instead of raising the raised-basket:
  if the basket-is-at-top is true:
    say "[dummy]";
  otherwise:
    now the basket-is-at-top is true;
    now the raised-basket is in Shaft Room;
    now the lowered-basket is in Drafty Room;
    say "The basket is raised to the top of the shaft."
-}

{-
Instead of raising the lowered-basket:
  if the basket-is-at-top is true:
    say "[dummy]";
  otherwise:
    now the basket-is-at-top is true;
    now the raised-basket is in Shaft Room;
    now the lowered-basket is in Drafty Room;
    say "The basket is raised to the top of the shaft."
-}

{-
Lowering is an action applying to one thing. Understand "lower [something]" as lowering.
Carry out lowering: say "You can't lower that."
-}

{-
Instead of lowering the raised-basket:
  if the basket-is-at-top is false:
    say "[dummy]";
  otherwise:
    now the basket-is-at-top is false;
    now the raised-basket is in Drafty Room;
    now the lowered-basket is in Shaft Room;
    say "The basket is lowered to the bottom of the shaft."
-}

{-
Instead of lowering the lowered-basket:
  if the basket-is-at-top is false:
    say "[dummy]";
  otherwise:
    now the basket-is-at-top is false;
    now the raised-basket is in Drafty Room;
    now the lowered-basket is in Shaft Room;
    say "The basket is lowered to the bottom of the shaft."
-}

{-
Machine-Room is a dark room. "This is a large, cold room whose sole exit is to the north. In one corner there is a machine which is reminiscent of a clothes dryer. On its face is a switch which is labelled [quotation mark]START[quotation mark]. The switch does not appear to be manipulable by any human hand (unless the fingers are about 1/16 by 1/4 inch). On the front of the machine is a large lid, which is [if the machine is open]open[otherwise]closed[end if]."
The printed name of Machine-Room is "Machine Room".
Machine-Room is in the Underground.
North of Machine-Room is Drafty Room.
-}

{-
The machine is a closed openable container in Machine-Room. The machine is scenery.
Understand "machine" and "pdp10" and "dryer" and "lid" as the machine.
The carrying capacity of the machine is 5.
The description of the machine is "It's a large machine with a lid and a switch."
-}

{-
Instead of taking the machine: say "It is far too large to carry."
Instead of opening the machine when the machine is open: say "[dummy]".
Instead of closing the machine when the machine is not open: say "[dummy]".
-}

{-
Report opening the machine:
  if the number of things in the machine is greater than 0:
    say "The lid opens, revealing [a list of things in the machine]." instead;
  say "The lid opens." instead.
-}

{-
Report closing the machine:
  say "The lid closes." instead.
-}

{-
The machine switch is scenery in Machine-Room. Understand "switch" as the machine switch.
The description of the machine switch is "It's a switch on the machine."
-}

{-
Does the player mean inserting something into the machine: it is very likely.
-}

{-
Instead of switching on the machine switch:
  if the player does not carry the screwdriver:
    say "It[apostrophe]s not clear how to turn it on with your bare hands." instead;
  if the machine is not closed:
    say "The machine must be closed first.";
  otherwise if the small pile of coal is in the machine:
    remove the small pile of coal from play;
    now the huge diamond is in the machine;
    play the sound of machine-sfx as sfx;
    say "The machine comes to life (figuratively) with a dazzling display of colored lights and bizarre noises. After a few moments, the excitement abates.";
  otherwise:
    let found-something be false;
    repeat with item running through things in the machine:
      now found-something is true;
      remove item from play;
      now the small piece of vitreous slag is in the machine;
    if found-something is true:
      play the sound of machine-sfx as sfx;
      say "The machine comes to life (figuratively) with a dazzling display of colored lights and bizarre noises. After a few moments, the excitement abates.";
    otherwise:
      say "The machine doesn[apostrophe]t seem to want to do anything."
-}

{-
The huge diamond is a thing. "There is an enormous diamond (perfectly cut) here."
Understand "diamond" and "huge" and "enormous" as the huge diamond.
The treasure-value of the huge diamond is 10.
The point-value of the huge diamond is 10.
-}

{-
The small piece of vitreous slag is a thing. Understand "gunk" and "piece" and "slag" and "small" and "vitreous" as the small piece of vitreous slag.
The description of the small piece of vitreous slag is "It's a small piece of vitreous slag."
-}

{-
Instead of taking the small piece of vitreous slag:
  say "The slag was rather insubstantial, and crumbles into dust at your touch.";
  remove the small piece of vitreous slag from play.
-}

{-
Instead of oiling the bolt:
  if the player carries the viscous material:
    say "Hmm. It appears the tube contained glue, not oil. Turning the bolt won't get any easier....";
  otherwise:
    say "You probably put spinach in your gas tank, too."
-}

{-
The granite-wall is a backdrop. The granite-wall is in Slide Room, North Temple, and Treasure Room.
The printed name of the granite-wall is "granite wall".
Understand "granite" and "wall" and "granite wall" as the granite-wall.
The description of the granite-wall is "[if the player is in Slide Room]It only SAYS [apostrophe]Granite Wall[apostrophe].[otherwise]The wall is solid granite here.[end if]".
-}

{-
Instead of taking or pushing or pulling the granite-wall:
  if the player is in Slide Room:
    say "The wall isn't granite.";
  otherwise:
    say "It's solid granite."
-}

{-
Slide Room is a dark room. "This is a small chamber, which appears to have been part of a coal mine. On the south wall of the chamber the letters 'Granite Wall' are etched in the rock. To the east is a long passage, and there is a steep metal slide twisting downward. To the north is a small opening."
Slide Room is in the Underground.
East of Slide Room is Cold Passage. North of Slide Room is Mine Entrance.
-}

{-
The slide-object is scenery in Slide Room. The printed name of the slide-object is "slide".
Understand "slide" and "metal" and "steep" as the slide-object.
The description of the slide-object is "It's a steep metal slide twisting downward."
-}

{-
Instead of entering the slide-object: try going down.
-}

{-
Instead of inserting something into the slide-object:
  if the noun is fixed in place:
    say "[yuks]";
  otherwise:
    say "The [noun] falls into the slide and is gone.";
    now the noun is in Cellar.
-}

{-
Instead of going down in Slide Room:
  say "You tumble down the slide....";
  if the cellar-visited is false:
    now the cellar-visited is true;
    increase the score by 25;
  move the player to Cellar.
-}

{-
The broken timber is in Timber Room. "There is a broken timber here."
Understand "timbers" and "pile" and "wooden" and "broken" as the broken timber.
The description of the broken timber is "They're just a pile of broken timbers."
-}
