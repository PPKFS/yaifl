module Yaifl.Zork.World.Mirrors where

{-

Chapter 14 - Mirror Rooms and Connecting Passages
Mirror Room 1 is a dark room. The printed name of Mirror Room 1 is "Mirror Room".
Mirror Room 1 is in the Underground.
The description of Mirror Room 1 is "You are in a large square room with tall ceilings. On the south wall is an enormous mirror which fills the entire wall. There are exits on the other three sides of the room.[if the mirror-mung is true][line break]Unfortunately, the mirror has been destroyed by your recklessness.[end if]".
North of Mirror Room 1 is Cold Passage. West of Mirror Room 1 is Twisting Passage. East of Mirror Room 1 is Small Cave.
The mirror-mung is a truth state that varies. The mirror-mung is false.
The mirror-one is scenery in Mirror Room 1. The printed name of the mirror-one is "mirror". Understand "mirror" and "reflection" and "enormous" as the mirror-one.
The description of the mirror-one is "[if the mirror-mung is true]The mirror is broken into many pieces.[otherwise]There is an ugly person staring back at you.[end if]"
Mirror Room 2 is a dark room. The printed name of Mirror Room 2 is "Mirror Room".
Mirror Room 2 is in the Underground.
The description of Mirror Room 2 is "You are in a large square room with tall ceilings. On the south wall is an enormous mirror which fills the entire wall. There are exits on the other three sides of the room.[if the mirror-mung is true][line break]Unfortunately, the mirror has been destroyed by your recklessness.[end if]".
West of Mirror Room 2 is Winding-Passage. North of Mirror Room 2 is Narrow Passage. East of Mirror Room 2 is Tiny Cave.
The mirror-two is scenery in Mirror Room 2. The printed name of the mirror-two is "mirror". Understand "mirror" and "reflection" and "enormous" as the mirror-two.
The description of the mirror-two is "[if the mirror-mung is true]The mirror is broken into many pieces.[otherwise]There is an ugly person staring back at you.[end if]"
Mirror-rubbing it with is an action applying to two things. Understand "rub [something] with [something]" and "touch [something] with [something]" as mirror-rubbing it with.
Carry out mirror-rubbing it with: say "That doesn[apostrophe]t seem to do anything."
Instead of mirror-rubbing the mirror-one with something when the mirror-mung is false:
	say "You feel a faint tingling transmitted through the [second noun]."
Instead of mirror-rubbing the mirror-two with something when the mirror-mung is false:
	say "You feel a faint tingling transmitted through the [second noun]."
Instead of rubbing the mirror-one:
	say "There is a rumble from deep within the earth and the room shakes.";
	move the player to Mirror Room 2, without printing a room description.
Instead of rubbing the mirror-two:
	say "There is a rumble from deep within the earth and the room shakes.";
	move the player to Mirror Room 1, without printing a room description.
Instead of taking the mirror-one: say "The mirror is many times your size. Give up."
Instead of taking the mirror-two: say "The mirror is many times your size. Give up."
Instead of attacking the mirror-one:
	if the mirror-mung is true:
		say "Haven't you done enough damage already?";
	otherwise:
		say "You have broken the mirror. I hope you have a seven years['] supply of good luck handy.";
		now the mirror-mung is true;
		now the lucky-flag is false.
Instead of attacking the mirror-two:
	if the mirror-mung is true:
		say "Haven't you done enough damage already?";
	otherwise:
		say "You have broken the mirror. I hope you have a seven years['] supply of good luck handy.";
		now the mirror-mung is true;
		now the lucky-flag is false.
Small Cave is a dark room. The printed name of Small Cave is "Cave". "This is a tiny cave with entrances west and north, and a staircase leading down."
Small Cave is in the Underground.
North of Small Cave is Mirror Room 1. Down from Small Cave is Atlantis Room. South of Small Cave is Atlantis Room. West of Small Cave is Twisting Passage.
Tiny Cave is a dark room. The printed name of Tiny Cave is "Cave". "This is a tiny cave with entrances west and north, and a dark, forbidding staircase leading down."
Tiny Cave is in the Underground.
North of Tiny Cave is Mirror Room 2. West of Tiny Cave is Winding-Passage. Down from Tiny Cave is Entrance to Hades.
Every turn when the player is in Tiny Cave and the location of the pair of candles is Tiny Cave and the pair of candles is lit (this is the drafty cave candle rule):
	if a random chance of 50 in 100 succeeds:
		now the pair of candles is not lit;
		say "A gust of wind blows out your candles![line break]";
		if in darkness:
			say "It is now completely dark.[line break]".
Cold Passage is a dark room. "This is a cold and damp corridor where a long east-west passageway turns into a southward path."
Cold Passage is in the Underground.
South of Cold Passage is Mirror Room 1. West of Cold Passage is Slide Room.
Narrow Passage is a dark room. "This is a long and narrow corridor where a long north-south passageway briefly narrows even further."
Narrow Passage is in the Underground.
North of Narrow Passage is Round Room. South of Narrow Passage is Mirror Room 2.
Winding-Passage is a dark room. "This is a winding passage. It seems that there are only exits on the east and north."
The printed name of Winding-Passage is "Winding Passage".
Winding-Passage is in the Underground.
North of Winding-Passage is Mirror Room 2. East of Winding-Passage is Tiny Cave.
Twisting Passage is a dark room. "This is a winding passage. It seems that there are only exits on the east and north."
Twisting Passage is in the Underground.
North of Twisting Passage is Mirror Room 1. East of Twisting Passage is Small Cave.
Atlantis Room is a dark room. "This is an ancient room, long under water. There is an exit to the south and a staircase leading up."
Atlantis Room is in the Underground.
Up from Atlantis Room is Small Cave. South of Atlantis Room is Reservoir-North.
The crystal trident is in Atlantis Room. "On the shore lies Poseidon's own crystal trident."
Understand "trident" and "fork" and "crystal" and "poseidon" as the crystal trident.
The treasure-value of the crystal trident is 11.
The point-value of the crystal trident is 4.
-}