module Yaifl.Zork.World.Underground where

{-
Part 3 - Underground Rooms
Chapter 1 - Cellar and Vicinity
Cellar is a dark room. "You are in a dark and damp cellar with a narrow passageway leading north, and a crawlway to the south. On the west is the bottom of a steep metal ramp which is unclimbable."
Cellar is in the Underground.
Instead of going west in Cellar:
	say "You try to ascend the ramp, but it is impossible, and you slide back down."
Instead of opening the trap door when the player is in Cellar:
	if the trap door is not open:
		say "The door is locked from above." instead.
Instead of closing the trap door when the player is in Cellar:
	if the trap door is open:
		say "The door closes and locks.";
		now the trap door is not open;
	otherwise:
		say "[dummy]"
After going down from Living Room to Cellar:
	if the trap-door-touched is false:
		now the trap-door-touched is true;
		now the trap door is not open;
		play the sound of trapdoor-sfx as sfx;
		say "The trap door crashes shut, and you hear someone barring it.[paragraph break]";
	continue the action.
North of Cellar is Troll-Room. South of Cellar is East-of-Chasm.
Chapter 2 - Troll-Room
Troll-Room is a dark room. The printed name of Troll-Room is "The Troll Room". "This is a small room with passages to the east and south and a forbidding hole leading west. Bloodstains and deep scratches (perhaps made by an axe) mar the walls."
Troll-Room is in the Underground.
Instead of going east in Troll-Room when the troll-flag is false:
	say "The troll fends you off with a menacing gesture."
Instead of going west in Troll-Room:
	if the troll-flag is false:
		say "The troll fends you off with a menacing gesture.";
	otherwise:
		move the player to Maze1.

Chapter 5 - East-of-Chasm
East-of-Chasm is a dark room. "You are on the east edge of a chasm, the bottom of which cannot be seen. A narrow passage goes north, and the path you are on continues to the east."
The printed name of East-of-Chasm is "East of Chasm".
East-of-Chasm is in the Underground.
East of East-of-Chasm is Gallery.
Instead of going down in East-of-Chasm:
	say "The chasm probably leads straight to the infernal regions."
Gallery is a dark room. "This is an art gallery. Most of the paintings have been stolen by vandals with exceptional taste. The vandals left through either the north or west exits."
Gallery is in the Underground.
North of Gallery is Studio.
Studio is a dark room. "This appears to have been an artist's studio. The walls and floors are splattered with paints of 69 different colors. Strangely enough, nothing of value is hanging here. At the south end of the room is an open door (also covered with paint). A dark and narrow chimney leads up from a fireplace; although you might be able to get up it, it seems unlikely you could get back down."
Studio is in the Underground.
The ZORK owner's manual is in Studio. "Loosely attached to a wall is a small piece of paper."
Understand "manual" and "piece" and "paper" and "zork" and "owner's" and "small" as the ZORK owner's manual.
The description of the ZORK owner's manual is "Congratulations![paragraph break]You are the privileged owner of ZORK I: The Great Underground Empire, a self-contained and self-maintaining universe. If used and maintained in accordance with normal operating practices for small universes, ZORK will provide many months of trouble-free operation."
The studio-door is scenery in Studio. The printed name of the studio-door is "door".
Understand "door" as the studio-door.
The description of the studio-door is "The door is covered with paint."
Instead of opening or closing the studio-door:
	say "The door won't budge."
Instead of entering the studio-door:
	try going south.
The studio-paint is scenery in Studio. The printed name of the studio-paint is "paint".
Understand "paint" and "paints" and "splatter" as the studio-paint.
The description of the studio-paint is "The paints are of 69 different colors."
Instead of attacking the studio-paint:
	say "Some paint chips away, revealing more paint."
The painting-damaged is a truth state that varies. The painting-damaged is false.
The painting is in Gallery.
Understand "painting" and "art" and "canvas" and "beautiful" as the painting.
The initial appearance of the painting is "[if the painting-damaged is true]There is a worthless piece of canvas here.[otherwise]Fortunately, there is still one chance for you to be a vandal, for on the far wall is a painting of unparalleled beauty.[end if]".
The description of the painting is "[if the painting-damaged is true]Worthless piece of canvas.[otherwise]This is a masterwork of painting. It depicts a serene scene of a farmhouse on a hillside.[end if]".
The treasure-value of the painting is 6.
The point-value of the painting is 4.
Instead of attacking the painting:
	now the painting-damaged is true;
	now the treasure-value of the painting is 0;
	now the point-value of the painting is 0;
	say "Congratulations! Unlike the other vandals, who merely stole the artist's masterpieces, you have destroyed one."

Chapter 7 - Grating and Leaves
The pile of leaves is in Grating Clearing. "On the ground is a pile of leaves."
Understand "leaves" and "leaf" and "pile" as the pile of leaves.
[ZIL LEAVES-APPEAR: reveals grate when leaves are disturbed]
To reveal-grate-from-leaves (this is the leaves-appear rule):
	if the grate-revealed is false and the grate is not open:
		now the grate-revealed is true;
		now the grate is zil-visible.
Instead of burning the pile of leaves:
	if the player carries the pile of leaves:
		die saying "The leaves burn, and so do you.";
	reveal-grate-from-leaves;
	if the grate-revealed is true and the grate is not open:
		say "With the leaves moved, a grating is revealed.[line break]";
	remove the pile of leaves from play;
	say "The leaves burn."
Instead of cutting the pile of leaves:
	say "You rustle the leaves around, making quite a mess.";
	if the grate-revealed is false:
		reveal-grate-from-leaves;
		say "[line break]With the leaves moved, a grating is revealed."
Instead of pushing the pile of leaves:
	say "Done.";
	if the grate-revealed is false:
		reveal-grate-from-leaves;
		say "[line break]In disturbing the pile of leaves, a grating is revealed."
Instead of taking the pile of leaves:
	if the grate-revealed is false:
		reveal-grate-from-leaves;
		say "In disturbing the pile of leaves, a grating is revealed.[line break]";
	continue the action.
Instead of looking under the pile of leaves:
	if the grate-revealed is false:
		say "Underneath the pile of leaves is a grating. As you release the leaves, the grating is once again concealed from view.";
	otherwise:
		say "There is nothing else under the leaves."
Chapter 8 - Cyclops-Room, Strange Passage, Treasure Room
Cyclops-Room is a dark room. Cyclops-Room is in the Underground.
The printed name of Cyclops-Room is "Cyclops Room".
The description of Cyclops-Room is "This room has an exit on the northwest, and a staircase leading up.[paragraph break][if the magic-flag is true]The east wall, previously solid, now has a cyclops-sized opening in it.[otherwise if the cyclops-asleep is true]The cyclops is sleeping blissfully at the foot of the stairs.[otherwise if the cyclops is in Cyclops-Room and the cyclops-wrath is 0]A cyclops, who looks prepared to eat horses (much less mere adventurers), blocks the staircase. From his state of health, and the bloodstains on the walls, you gather that he is not very friendly, though he likes people.[otherwise if the cyclops is in Cyclops-Room and the cyclops-wrath > 0]The cyclops is standing in the corner, eyeing you closely. I don't think he likes you very much. He looks extremely hungry, even for a cyclops.[otherwise if the cyclops is in Cyclops-Room and the cyclops-fed is true]The cyclops, having eaten the hot peppers, appears to be gasping. His enflamed tongue protrudes from his man-sized mouth.[end if]".
Instead of going east in Cyclops-Room:
	if the magic-flag is true:
		move the player to Strange Passage;
	otherwise:
		say "The east wall is solid rock."
Instead of going up in Cyclops-Room:
	if the cyclops-flag is true:
		move the player to Treasure Room;
		if the treasure-room-visited is false:
			now the treasure-room-visited is true;
			increase the score by 25;
	otherwise:
		say "The cyclops doesn[apostrophe]t look like he'll let you past."
Strange Passage is a dark room. "This is a long passage. To the west is one entrance. On the east there is an old wooden door, with a large opening in it (about cyclops sized)."
Strange Passage is in the Underground.
West of Strange Passage is Cyclops-Room. East of Strange Passage is Living Room.
Treasure Room is a dark room. "This is a large room, whose east wall is solid granite. A number of discarded bags, which crumble at your touch, are scattered about on the floor. There is an exit down a staircase."
Treasure Room is in the Underground.
Down from Treasure Room is Cyclops-Room.

Chapter 10 - East-West Passage and Round Room Area
East-West Passage is a dark room. "This is a narrow east-west passageway. There is a narrow stairway leading down at the north end of the room."
East-West Passage is in the Underground.
[East of Troll-Room is established by "West of East-West Passage is Troll-Room" below]
East of East-West Passage is Round Room. West of East-West Passage is Troll-Room. Down from East-West Passage is Chasm.
North of East-West Passage is Chasm.
Round Room is a dark room. "This is a circular stone room with passages in all directions. Several of them have unfortunately been blocked by cave-ins."
Round Room is in the Underground.
East of Round Room is Loud Room. North of Round Room is North-South Passage. South of Round Room is Narrow Passage. Southeast of Round Room is Engravings Cave.

-}