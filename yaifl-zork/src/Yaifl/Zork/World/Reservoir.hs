module Yaifl.Zork.World.Reservoir where

{-
TODO
Chapter 11 - Dam and Reservoir Area
Deep Canyon is a dark room. "You are on the south edge of a deep canyon. Passages lead off to the east, northwest and southwest. A stairway leads down. [if the gates-open is true and the low-tide is false]You can hear a loud roaring sound, like that of rushing water, from below.[otherwise if the gates-open is false and the low-tide is true][otherwise]You can hear the sound of flowing water from below.[end if]".
Deep Canyon is in the Underground.
Northwest of Deep Canyon is Reservoir-South. East of Deep Canyon is Dam-Room. Southwest of Deep Canyon is North-South Passage. Down from Deep Canyon is Loud Room.
Loud Room is a dark room. Loud Room is in the Underground.
East of Loud Room is Damp Cave. West of Loud Room is Round Room. Up from Loud Room is Deep Canyon.
The loud-room-quiet is a truth state that varies. The loud-room-quiet is false.
The description of Loud Room is "This is a large room with a ceiling which cannot be detected from the ground. There is a narrow passage from east to west and a stone stairway leading upward.[if the loud-room-quiet is true or (the gates-open is false and the low-tide is true)] The room is eerie in its quietness.[otherwise] The room is deafeningly loud with an undetermined rushing sound. The sound seems to reverberate from all of the walls, making it difficult even to think.[end if]".
The platinum bar is in Loud Room. "On the ground is a large platinum bar."
Understand "bar" and "platinum" and "large" as the platinum bar.
The treasure-value of the platinum bar is 5.
The point-value of the platinum bar is 10.
Instead of taking the platinum bar when the loud-room-quiet is false:
  say "The acoustics of the room change as the platinum bar is carried through it. Unfortunately, the unpleasant consequences of this action are that the room now reflects sound more perfectly, and the painful clanging increases to an unbearable level. You stagger and drop the bar, and run from the room.";
  move the player to Round Room.
Echoing is an action applying to nothing.
Understand "echo" as echoing.
Carry out echoing:
  if the player is in Loud Room:
    if the loud-room-quiet is false:
      now the loud-room-quiet is true;
      say "The acoustics of the room change subtly.";
    otherwise:
      say "echo echo ...";
  otherwise:
    say "echo echo ..."
Every turn when the player is in Loud Room and the gates-open is true and the low-tide is false (this is the loud room ejection rule):
  say "It is unbearably loud here, with an ear-splitting roar seeming to come from all around you. There is a pounding in your head which won't stop. With a tremendous effort, you scramble out of the room.";
  let roll be a random number between 1 and 3;
  if roll is 1:
    move the player to Round Room;
  otherwise if roll is 2:
    move the player to Damp Cave;
  otherwise:
    move the player to Deep Canyon.
Damp Cave is a dark room. "This cave has exits to the west and east, and narrows to a crack toward the south. The earth is particularly damp here."
Damp Cave is in the Underground.
The damp-crack is scenery in Damp Cave. The printed name of the damp-crack is "crack".
Understand "crack" and "narrow" as the damp-crack.
The description of the damp-crack is "The crack is very narrow."
Instead of entering the damp-crack: say "You can't fit through the crack."
Instead of going south in Damp Cave:
  say "It is too narrow for most insects."
North-South Passage is a dark room. "This is a high north-south passage, which forks to the northeast."
North-South Passage is in the Underground.
North of North-South Passage is Chasm. Northeast of North-South Passage is Deep Canyon. South of North-South Passage is Round Room.
Chasm is a dark room. The printed name of Chasm is "Chasm". "A chasm runs southwest to northeast and the path follows it. You are on the south side of the chasm, where a crack opens into a passage."
Chasm is in the Underground.
Northeast of Chasm is Reservoir-South. Southwest of Chasm is East-West Passage. Up from Chasm is East-West Passage. South of Chasm is North-South Passage.
Instead of going down in Chasm:
  say "Are you out of your mind?"
The chasm-pseudo is a backdrop. The chasm-pseudo is in East-of-Chasm, Reservoir-South, and Chasm.
The printed name of the chasm-pseudo is "chasm".
Understand "chasm" and "abyss" as the chasm-pseudo.
The description of the chasm-pseudo is "The chasm is deep and impassable."
Crossing is an action applying to one thing. Understand "cross [something]" and "cross over [something]" as crossing.
Carry out crossing: say "You can't cross that!"
Instead of jumping when the player is in East-of-Chasm or the player is in Chasm:
  say "You look before leaping, and realize that you would never survive."
Instead of jumping in Dome Room:
  if the dome-flag is true:
    continue the action;
  say "This was not a very safe place to try jumping.";
  die saying "[jumploss]"
[ZIL V-LEAP: Kitchen chimney shaft — DOWN TO STUDIO IF FALSE-FLAG (always blocked)]
Instead of jumping in Kitchen:
  say "This was not a very safe place to try jumping.";
  die saying "[jumploss]"
[ZIL V-LEAP: Altar — DOWN TO TINY-CAVE IF COFFIN-CURE (blocked when carrying coffin)]
Instead of jumping in South Temple:
  if the player carries the gold coffin:
    say "This was not a very safe place to try jumping.";
    die saying "[jumploss]";
  continue the action.
[ZIL V-LEAP: Up-a-Tree — special non-fatal case, safely jump down]
Instead of jumping in Up a Tree:
  say "In a feat of unaccustomed daring, you manage to land on your feet without killing yourself.";
  try going down.
Instead of jumping in Canyon View:
  die saying "Nice view, lousy place to jump."
Instead of crossing the chasm-pseudo:
  say "It's too far to jump, and there's no bridge."
Instead of inserting something into the chasm-pseudo:
  say "The [noun] drops out of sight into the chasm.";
  remove the noun from play.
Reservoir-South is a dark room. The printed name of Reservoir-South is "Reservoir South". Reservoir-South is in the Underground.
The description of Reservoir-South is "[if the low-tide is true and the gates-open is true]You are in a long room, to the north of which was formerly a lake. However, with the water level lowered, there is merely a wide stream running through the center of the room.[otherwise if the gates-open is true]You are in a long room. To the north is a large lake, too deep to cross. You notice, however, that the water level appears to be dropping at a rapid rate. Before long, it might be possible to cross to the other side from here.[otherwise if the low-tide is true]You are in a long room, to the north of which is a wide area which was formerly a reservoir, but now is merely a stream. You notice, however, that the level of the stream is rising quickly and that before long it will be impossible to cross here.[otherwise]You are in a long room on the south shore of a large lake, far too deep and wide for crossing.[end if][paragraph break]There is a path along the stream to the east or west, a steep pathway climbing southwest along the edge of a chasm, and a path leading into a canyon to the southeast.".
Southeast of Reservoir-South is Deep Canyon.
The lake-pseudo is a backdrop. The lake-pseudo is in Reservoir-South and Reservoir-North.
The printed name of the lake-pseudo is "lake".
Understand "lake" and "reservoir" as the lake-pseudo when the player is in Reservoir-South or the player is in Reservoir-North.
The description of the lake-pseudo is "[if the low-tide is true]There's not much lake left....[otherwise]The lake stretches out before you.[end if]"
Instead of crossing the lake-pseudo:
  say "It's too wide to cross."
Instead of entering the lake-pseudo:
  say "You can't swim in this lake."
Instead of swimming when the player is in Reservoir-South or the player is in Reservoir-North:
  say "You can't swim in this lake."
East of Reservoir-South is Dam-Room. West of Reservoir-South is Stream View.
Southwest of Reservoir-South is Chasm.
Instead of going north in Reservoir-South:
  if the low-tide is true:
    move the player to Reservoir;
  otherwise:
    say "You would drown."
Dam-Room is a dark room. The printed name of Dam-Room is "Dam".
Dam-Room is in the Underground.
The description of Dam-Room is "You are standing on the top of Flood Control Dam #3, which was quite a tourist attraction in times far distant. There are paths to the north, south, and west, and a scramble down.[paragraph break][if the low-tide is true and the gates-open is true]The water level behind the dam is low: The sluice gates have been opened. Water rushes through the dam and downstream.[otherwise if the gates-open is true]The sluice gates are open, and water rushes through the dam. The water level behind the dam is still high.[otherwise if the low-tide is true]The sluice gates are closed. The water level in the reservoir is quite low, but the level is rising quickly.[otherwise]The sluice gates on the dam are closed. Behind the dam, there can be seen a wide reservoir. Water is pouring over the top of the now abandoned dam.[end if][paragraph break]There is a control panel here, on which a large metal bolt is mounted. Directly above the bolt is a small green plastic bubble[if the gate-flag is true] which is glowing serenely[end if]."
South of Dam-Room is Deep Canyon. Down from Dam-Room is Dam-Base. East of Dam-Room is Dam-Base. North of Dam-Room is Dam-Lobby.
West of Dam-Room is Reservoir-South.
The dam is scenery in Dam-Room. Understand "dam" and "gate" and "gates" and "fcd" and "fcd#3" and "fcd3" as the dam.
The description of the dam is "This is Flood Control Dam #3, quite an impressive engineering feat."
Instead of opening or closing the dam: say "Sounds reasonable, but this isn't how."
Instead of plugging the dam with something:
  if the second noun is the viscous material:
    say "Are you the little Dutch boy, then? Sorry, this is a big dam.";
  otherwise:
    say "With a [second noun]? Do you know how big this dam is? You could only stop a tiny leak with that."
The bolt is scenery in Dam-Room. Understand "bolt" and "nut" and "metal" and "large" as the bolt.
The description of the bolt is "It's a large metal bolt attached to the dam structure."
Instead of taking the bolt: say "It is an integral part of the control panel."
The green bubble is scenery in Dam-Room. Understand "bubble" and "small" and "green" and "plastic" as the green bubble.
The description of the green bubble is "A small green plastic bubble is floating in the stream."
Instead of taking the green bubble: say "It is an integral part of the control panel."
The control panel is scenery in Dam-Room. Understand "panel" and "control" as the control panel.
The description of the control panel is "The control panel is part of the dam infrastructure."
Dam-Lobby is a dark room. The printed name of Dam-Lobby is "Dam Lobby". "This room appears to have been the waiting room for groups touring the dam. There are open doorways here to the north and east marked 'Private', and there is a path leading south over the top of the dam."
Dam-Lobby is in the Underground.
South of Dam-Lobby is Dam-Room. North of Dam-Lobby is Maintenance Room. East of Dam-Lobby is Maintenance Room.
The tour guidebook is in Dam-Lobby. "Some guidebooks entitled 'Flood Control Dam #3' are on the reception desk."
Understand "guide" and "book" and "guidebooks" and "tour" as the tour guidebook.
The description of the tour guidebook is "[fixed letter spacing]   Flood Control Dam #3[line break][line break]FCD#3 was constructed in year 783 of the Great Underground Empire to harness the mighty Frigid River. This work was supported by a grant of 37 million zorkmids from your omnipotent local tyrant Lord Dimwit Flathead the Excessive. This impressive structure is composed of 370,000 cubic feet of concrete, is 256 feet tall at the center, and 193 feet wide at the top. The lake created behind the dam has a volume of 1.7 billion cubic feet, an area of 12 million square feet, and a shore line of 36 thousand feet.[line break][line break]The construction of FCD#3 took 112 days from ground breaking to the dedication. It required a work force of 384 slaves, 34 slave drivers, 12 engineers, 2 turtle doves, and a partridge in a pear tree. The work was managed by a command team composed of 2345 bureaucrats, 2347 secretaries (at least two of whom could type), 12,256 paper shufflers, 52,469 rubber stampers, 245,193 red tape processors, and nearly one million dead trees.[line break][line break]We will now point out some of the more interesting features of FCD#3 as we conduct you on a guided tour of the facilities:[line break][line break]      1) You start your tour here in the Dam-Lobby. You will notice on your right that....[variable letter spacing]"
The matchbook is in Dam-Lobby. "There is a matchbook whose cover says 'Visit Beautiful FCD#3' here."
Understand "match" and "matches" and "matchbook" as the matchbook.
The description of the matchbook is "The matchbook isn[apostrophe]t very interesting, except for what[apostrophe]s written on it."
The match-count is a number that varies. The match-count is 6.
Instead of examining the matchbook when the match-lit is true:
  say "The match is burning."
Reading is an action applying to one thing. Understand "read [something]" as reading.
Instead of reading the matchbook:
  say "[fixed letter spacing](Close cover before striking)[line break][line break]YOU too can make BIG MONEY in the exciting field of PAPER SHUFFLING![line break][line break]Mr. Anderson of Muddle, Mass. says: 'Before I took this course I was a lowly bit twiddler. Now with what I learned at GUE Tech I feel really important and can obfuscate and confuse with the best.'[line break][line break]Dr. Blank had this to say: 'Ten short days ago all I could look forward to was a dead-end job as a doctor. Now I have a promising future and make really big Zorkmids.'[line break][line break]GUE Tech can't promise these fantastic results to everyone. But when you earn your degree from GUE Tech, your future will be brighter.[variable letter spacing]"
Maintenance Room is a dark room. "This is what appears to have been the maintenance room for Flood Control Dam #3. Apparently, this room has been ransacked recently, for most of the valuable equipment is gone. On the wall in front of you is a group of buttons colored blue, yellow, brown, and red. There are doorways to the west and south."
Maintenance Room is in the Underground.
South of Maintenance Room is Dam-Lobby. West of Maintenance Room is Dam-Lobby.
The yellow button is scenery in Maintenance Room. Understand "yellow" and "button" as the yellow button.
The brown button is scenery in Maintenance Room. Understand "brown" and "button" as the brown button.
The red button is scenery in Maintenance Room. Understand "red" and "button" as the red button.
The blue button is scenery in Maintenance Room. Understand "blue" and "button" as the blue button.
Instead of examining the yellow button: say "They[apostrophe]re greek to you."
Instead of examining the brown button: say "They[apostrophe]re greek to you."
Instead of examining the red button: say "They[apostrophe]re greek to you."
Instead of examining the blue button: say "They[apostrophe]re greek to you."
Instead of pushing the yellow button:
  now the gate-flag is true;
  say "Click."
Instead of pushing the brown button:
  now the gate-flag is false;
  say "Click."
Instead of pushing the red button:
  say "The lights within the room come on."
The water-level is a number that varies. The water-level is 0.
The maint-flooded is a truth state that varies. The maint-flooded is false.
The leak is scenery. The leak is zil-invisible.
Understand "leak" and "pipe" and "pipes" and "stream" and "water" as the leak.
The description of the leak is "Water is pouring out of a crack in the east wall."
Instead of pushing the blue button:
  if the water-level is 0:
    now the water-level is 1;
    now the leak is zil-visible;
    now the leak is in Maintenance Room;
    say "There is a rumbling sound and a stream of water appears to burst from the east wall of the room (apparently, a leak has occurred in a pipe).";
  otherwise:
    say "The blue button appears to be jammed."
Plugging it with is an action applying to two things. Understand "plug [something] with [something]" and "fix [something] with [something]" and "patch [something] with [something]" as plugging it with.
Carry out plugging it with: say "That doesn't work."
Instead of plugging the leak with the viscous material:
  if the water-level > 0:
    now the water-level is -1;
    say "By some miracle of Zorkian technology, you have managed to stop the leak in the dam."
Instead of putting the viscous material on the leak:
  try plugging the leak with the viscous material.
Every turn when the water-level > 0 and the maint-flooded is false (this is the maintenance flooding rule):
  increase the water-level by 1;
  if the player is in Maintenance Room:
    if the water-level is 2:
      say "The water level here is now up to your ankles.[line break]";
    otherwise if the water-level is 4:
      say "The water level here is now up to your shin.[line break]";
    otherwise if the water-level is 6:
      say "The water level here is now up to your knees.[line break]";
    otherwise if the water-level is 8:
      say "The water level here is now up to your hips.[line break]";
    otherwise if the water-level is 10:
      say "The water level here is now up to your waist.[line break]";
    otherwise if the water-level is 12:
      say "The water level here is now up to your chest.[line break]";
    otherwise if the water-level is 13:
      say "The water level here is now up to your neck.[line break]";
  if the water-level is at least 14:
    now the maint-flooded is true;
    if the player is in Maintenance Room:
      die saying "I'm afraid you have done drowned yourself."
Instead of going to Maintenance Room when the maint-flooded is true:
  say "The room is full of water and cannot be entered." instead.
The tool chests are in Maintenance Room. "There is a group of tool chests here."
The tool chests are plural-named.
Understand "chest" and "chests" and "tool" and "toolchests" and "group" as the tool chests.
The description of the tool chests is "The chests are all empty."
Instead of taking the tool chests:
  remove the tool chests from play;
  say "The chests are so rusty and corroded that they crumble when you touch them."
Instead of opening the tool chests:
  remove the tool chests from play;
  say "The chests are so rusty and corroded that they crumble when you touch them."
Instead of inserting something into the tool chests:
  remove the tool chests from play;
  say "The chests are so rusty and corroded that they crumble when you touch them."
The wrench is in Maintenance Room. "There is a wrench here." Understand "wrench" and "tool" as the wrench.
The description of the wrench is "It's a wrench."
The screwdriver is in Maintenance Room. "There is a screwdriver here." Understand "screwdriver" and "tool" and "driver" as the screwdriver.
The description of the screwdriver is "It's a screwdriver."
The tube is in Maintenance Room. "There is an object which looks like a tube of toothpaste here."
Understand "tube" and "tooth" and "paste" as the tube.
The tube is a closed openable container. The carrying capacity of the tube is 1.
The description of the tube is "The label reads: 'Frobozz Magic Gunk Company --- All-Purpose Gunk'."
Instead of inserting something into the tube: say "The tube refuses to accept anything."
Instead of squeezing the tube:
  if the tube is open:
    if the viscous material is in the tube:
      now the player carries the viscous material;
      say "The viscous material oozes into your hand.";
    otherwise:
      say "The tube is apparently empty.";
  otherwise:
    say "The tube is closed."
The viscous material is in the tube. Understand "material" and "gunk" and "viscous" and "putty" as the viscous material.
The description of the viscous material is "It's a viscous, putty-like material."
The group of tool chests is scenery in Maintenance Room. Understand "chest" and "chests" and "group" and "toolchests" and "tool" as the group of tool chests.
The description of the group of tool chests is "The chests are all empty."
Instead of taking or opening the group of tool chests:
  remove the group of tool chests from play;
  say "The chests are so rusty and corroded that they crumble when you touch them."
Instead of inserting something into the group of tool chests:
  remove the group of tool chests from play;
  say "The chests are so rusty and corroded that they crumble when you touch them."
Chapter 12 - Reservoir Area
Reservoir is a dark room. Reservoir is in the Underground.
The description of Reservoir is "[if the low-tide is true]You are on what used to be a large lake, but which is now a large mud pile. There are [quotation mark]shores[quotation mark] to the north and south.[otherwise]You are on the lake. Beaches can be seen north and south. Upstream a small stream enters the lake through a narrow cleft in the rocks. The dam can be seen downstream.[end if]".
North of Reservoir is Reservoir-North. South of Reservoir is Reservoir-South.
Reservoir-North is a dark room. The printed name of Reservoir-North is "Reservoir North".
The description of Reservoir-North is "[if the low-tide is true and the gates-open is true]You are in a large cavernous room, the south of which was formerly a lake. However, with the water level lowered, there is merely a wide stream running through there.[otherwise if the gates-open is true]You are in a large cavernous area. To the south is a wide lake, whose water level appears to be falling rapidly.[otherwise if the low-tide is true]You are in a cavernous area, to the south of which is a very wide stream. The level of the stream is rising rapidly, and it appears that before long it will be impossible to cross to the other side.[otherwise]You are in a large cavernous room, north of a large lake.[end if][paragraph break]There is a slimy stairway leaving the room to the north."
Reservoir-North is in the Underground.
North of Reservoir-North is Atlantis Room.
Instead of going south in Reservoir-North:
  if the low-tide is true:
    move the player to Reservoir;
  otherwise:
    say "You would drown."
Stream View is a dark room. "You are standing on a path beside a gently flowing stream. The path follows the stream, which flows from west to east."
Stream View is in the Underground.
East of Stream View is Reservoir-South.
Instead of going west in Stream View:
  say "The stream emerges from a spot too small for you to enter."
The stream-pseudo is a backdrop. The stream-pseudo is in Stream View and In-Stream.
The printed name of the stream-pseudo is "stream".
Understand "stream" as the stream-pseudo.
The description of the stream-pseudo is "The stream flows gently from west to east."
Instead of entering the stream-pseudo:
  say "You can't swim in the stream."
Instead of crossing the stream-pseudo:
  say "The other side is a sheer rock cliff."
Instead of swimming when the player is in Stream View or the player is in In-Stream:
  say "You can't swim in the stream."
The hand-held air pump is in Reservoir-North. "There is a hand-held air pump here."
Understand "pump" and "air-pump" and "tool" and "small" and "hand-held" as the hand-held air pump.
The description of the hand-held air pump is "It's a small hand-held air pump."
Chapter 13 - Dam-Base and River
Dam-Base is a room. The printed name of Dam-Base is "Dam Base". "You are at the base of Flood Control Dam #3, which looms above you and to the north. The river Frigid is flowing by here. Along the river are the White Cliffs which seem to form giant walls stretching from north to south along the shores of the river as it winds its way downstream."
Dam-Base is in the Underground.
North of Dam-Base is Dam-Room. Up from Dam-Base is Dam-Room.
The pile of plastic is in Dam-Base. "There is a folded pile of plastic here which has a small valve attached."
Understand "boat" and "pile" and "plastic" and "valve" and "inflatable" as the pile of plastic.
The description of the pile of plastic is "It's a pile of folded plastic with a small valve attached."
-}