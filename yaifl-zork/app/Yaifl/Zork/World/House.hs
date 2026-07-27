module Yaifl.Zork.World.House where

{-

Section 2 - Rooms Outside the House
West-of-House is a room. "You are standing in an open field west of a white house, with a boarded front door.[if the won-flag is true] A secret path leads southwest into the forest.[end if]".
The printed name of West-of-House is "West of House".
West-of-House is in House Exterior.
The white house is a backdrop. The white house is in House Exterior and Forest Area. The description of the white house is "The house is a beautiful colonial house which is painted white. It is clear that the owners must have been extremely wealthy."
Understand "house" and "white" and "beautiful" and "colonial" as the white house.
Instead of burning the white house:
	say "You must be joking."
Instead of taking or pushing or pulling or touching the white house when the location of the player is not in House Exterior:
	say "You're not at the house."
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
Instead of entering the white house when the location of the player is Behind House:
	if the kitchen-window is open:
		try going west;
	otherwise:
		say "The window is closed."
Instead of entering the white house when the location of the player is in House Exterior and the location of the player is not Behind House:
	say "I can't see how to get in from here."
North-of-House is a room. "You are facing the north side of a white house. There is no door here, and all the windows are boarded up. To the north a narrow path winds through the trees."
The printed name of North-of-House is "North of House".
North-of-House is in House Exterior.
South-of-House is a room. "You are facing the south side of a white house. There is no door here, and all the windows are boarded."
The printed name of South-of-House is "South of House".
South-of-House is in House Exterior.
Behind House is a room. The printed name of Behind House is "Behind House".
Behind House is in House Exterior.
The description of Behind House is "You are behind the white house. A path leads into the forest to the east. In one corner of the house there is a small window which is [if the kitchen-window is open]open[otherwise]slightly ajar[end if]."
Section 3 - Map Connections Around the House
North-of-House is north of West-of-House. South-of-House is south of West-of-House.
Northeast of West-of-House is North-of-House. Southeast of West-of-House is South-of-House.
North of Behind House is North-of-House. South of Behind House is South-of-House.
Southwest of Behind House is South-of-House. Northwest of Behind House is North-of-House.
East of South-of-House is Behind House. West of South-of-House is West-of-House.
Northeast of South-of-House is Behind House. Northwest of South-of-House is West-of-House.
East of North-of-House is Behind House. West of North-of-House is West-of-House.
East of Behind House is Clearing.
Instead of going east in West-of-House:
	say "The door is boarded and you can't remove the boards."
The boarded-windows are a backdrop. The boarded-windows are in North-of-House and South-of-House.
The printed name of the boarded-windows is "boarded window".
Understand "window" and "windows" and "boarded" as the boarded-windows.
The description of the boarded-windows is "The windows are all boarded up."
Instead of opening the boarded-windows: say "The windows are boarded and can[apostrophe]t be opened."
Instead of attacking the boarded-windows: say "You can[apostrophe]t break the windows open."
Instead of going south in North-of-House:
	say "The windows are all boarded."
Instead of going north in South-of-House:
	say "The windows are all boarded."


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
The kitchen-window is a door. The kitchen-window is not open. The kitchen-window is scenery.
The printed name of the kitchen-window is "kitchen window".
Understand "window" and "kitchen" and "small" as the kitchen-window.
The kitchen-window is west of Behind House and east of Kitchen.
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