module Yaifl.Zork.World.Maze where

{-
Chapter 6 - Maze
Maze1 is a dark room. The printed name of Maze1 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze1 is in the Underground.
North of Maze1 is Maze1. South of Maze1 is Maze2. West of Maze1 is Maze4.
Maze2 is a dark room. The printed name of Maze2 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze2 is in the Underground.
South of Maze2 is Maze1. East of Maze2 is Maze3.
Instead of going down in Maze2:
	say "You won[apostrophe]t be able to get back up to the tunnel you are going through when it gets to the next room.";
	move the player to Maze4.
Maze3 is a dark room. The printed name of Maze3 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze3 is in the Underground.
West of Maze3 is Maze2. North of Maze3 is Maze4. Up from Maze3 is Maze5.
Maze4 is a dark room. The printed name of Maze4 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze4 is in the Underground.
West of Maze4 is Maze3. North of Maze4 is Maze1. East of Maze4 is Dead End 1.
Dead End 1 is a dark room. The printed name of Dead End 1 is "Dead End". "You have come to a dead end in the maze."
Dead End 1 is in the Underground.
South of Dead End 1 is Maze4.
Maze5 is a dark room. The printed name of Maze5 is "Maze". "This is part of a maze of twisty little passages, all alike.[line break]A skeleton, probably the remains of a luckless adventurer, lies here."
Maze5 is in the Underground.
East of Maze5 is Dead End 2. North of Maze5 is Maze3. Southwest of Maze5 is Maze6.
The skeleton is scenery in Maze5.  Understand "bones" and "skeleton" and "body" as the skeleton.
The description of the skeleton is "It's a skeleton, probably the remains of a luckless adventurer."
Instead of taking the skeleton:
	skeleton-curse.
Instead of pushing the skeleton:
	skeleton-curse.
Instead of attacking the skeleton:
	skeleton-curse.
Instead of rubbing the skeleton:
	skeleton-curse.
Instead of raising the skeleton:
	skeleton-curse.
Instead of lowering the skeleton:
	skeleton-curse.
Instead of touching the skeleton:
	skeleton-curse.
Instead of kicking the skeleton:
	skeleton-curse.
Instead of kissing the skeleton:
	skeleton-curse.
To skeleton-curse:
	say "A ghost appears in the room and is appalled at your desecration of the remains of a fellow adventurer. He casts a curse on your valuables and banishes them to the Land of the Living Dead. The ghost leaves, muttering obscenities.";
	repeat with item running through things carried by the player:
		if the treasure-value of item is greater than 0:
			now item is in Land of the Dead;
	if in darkness:
		say "[line break]It's pitch black in here!".
The rusty knife is in Maze5. "Beside the skeleton is a rusty knife."
Understand "knife" and "rusty" as the rusty knife.
The rusty knife is a weapon.
The burned-out lantern is in Maze5. "The deceased adventurer's useless lantern is here."
Understand "lantern" and "lamp" and "rusty" and "burned" and "dead" and "useless" as the burned-out lantern when the burned-out lantern is visible.
The skeleton key is in Maze5. "There is a skeleton key here."
Understand "key" and "skeleton" as the skeleton key.
The description of the skeleton key is "It's a rusty old skeleton key."
The leather bag of coins is in Maze5. "An old leather bag, bulging with coins, is here."
Understand "bag" and "coins" and "old" and "leather" as the leather bag of coins.
The treasure-value of the leather bag of coins is 5.
The point-value of the leather bag of coins is 10.
Instead of opening the leather bag of coins: say "The coins are safely inside; there[apostrophe]s no need to do that."
Instead of closing the leather bag of coins: say "The coins are safely inside; there[apostrophe]s no need to do that."
Instead of inserting something into the leather bag of coins: say "Don[apostrophe]t be silly. It wouldn[apostrophe]t be a leather bag of coins anymore."
Instead of searching the leather bag of coins: say "There are lots of coins in there."
Dead End 2 is a dark room. The printed name of Dead End 2 is "Dead End". "You have come to a dead end in the maze."
Dead End 2 is in the Underground.
West of Dead End 2 is Maze5.
Maze6 is a dark room. The printed name of Maze6 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze6 is in the Underground.
Down from Maze6 is Maze5. East of Maze6 is Maze7. West of Maze6 is Maze6. Up from Maze6 is Maze9.
Maze7 is a dark room. The printed name of Maze7 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze7 is in the Underground.
Up from Maze7 is Maze14. West of Maze7 is Maze6. East of Maze7 is Maze8. South of Maze7 is Maze15.
Instead of going down in Maze7:
	say "You won[apostrophe]t be able to get back up to the tunnel you are going through when it gets to the next room.";
	move the player to Dead End 1.
Maze8 is a dark room. The printed name of Maze8 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze8 is in the Underground.
Northeast of Maze8 is Maze7. West of Maze8 is Maze8. Southeast of Maze8 is Dead End 3.
Dead End 3 is a dark room. The printed name of Dead End 3 is "Dead End". "You have come to a dead end in the maze."
Dead End 3 is in the Underground.
North of Dead End 3 is Maze8.
Maze9 is a dark room. The printed name of Maze9 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze9 is in the Underground.
North of Maze9 is Maze6. East of Maze9 is Maze10. South of Maze9 is Maze13. West of Maze9 is Maze12. Northwest of Maze9 is Maze9.
Instead of going down in Maze9:
	say "You won[apostrophe]t be able to get back up to the tunnel you are going through when it gets to the next room.";
	move the player to Maze11.
Maze10 is a dark room. The printed name of Maze10 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze10 is in the Underground.
East of Maze10 is Maze9. West of Maze10 is Maze13. Up from Maze10 is Maze11.
Maze11 is a dark room. The printed name of Maze11 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze11 is in the Underground.
Northeast of Maze11 is Grating Room. Down from Maze11 is Maze10. Northwest of Maze11 is Maze13. Southwest of Maze11 is Maze12.
Grating Room is a dark room.
The description of Grating Room is "You are in a small room near the maze. There are twisty passages in the immediate vicinity.[if the grate is open][line break]Above you is an open grating with sunlight pouring in.[otherwise if the grate is not locked][line break]Above you is a grating.[otherwise][line break]Above you is a grating locked with a skull-and-crossbones lock.[end if]".
Grating Room is in the Underground.
Southwest of Grating Room is Maze11.
The grate is a door. The grate is scenery. The grate is closed and openable and lockable and locked. The matching key of the grate is the skeleton key.
Understand "grate" and "grating" as the grate.
The grate is above Grating Room and below Grating Clearing.
Instead of locking the grate with something when the grate is open:
	say "You can[apostrophe]t lock an open grate."
Instead of locking the grate with something when the player is in Grating Clearing:
	say "You can[apostrophe]t lock it from this side."
Instead of unlocking the grate with something when the player is in Grating Clearing:
	say "You can[apostrophe]t reach the lock from here."
Instead of unlocking the grate with something when the second noun is not the skeleton key:
	say "Can you unlock a grating with a [second noun]?"
Instead of inserting something into the grate when the grate is open and the player is in Grating Clearing:
	if the noun is the coffin or the noun is the trunk of jewels:
		say "It won[apostrophe]t fit through the grating.";
	otherwise:
		now the noun is in Grating Room;
		say "The [noun] goes through the grating into the darkness below."
Instead of going up in Grating Room:
	if the grate is not open:
		say "The grating is closed." instead;
	move the player to Grating Clearing.
Maze12 is a dark room. The printed name of Maze12 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze12 is in the Underground.
Southwest of Maze12 is Maze11. East of Maze12 is Maze13. Up from Maze12 is Maze9. North of Maze12 is Dead End 4.
Instead of going down in Maze12:
	say "You won[apostrophe]t be able to get back up to the tunnel you are going through when it gets to the next room.";
	move the player to Maze5.
Dead End 4 is a dark room. The printed name of Dead End 4 is "Dead End". "You have come to a dead end in the maze."
Dead End 4 is in the Underground.
South of Dead End 4 is Maze12.
Maze13 is a dark room. The printed name of Maze13 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze13 is in the Underground.
East of Maze13 is Maze9. Down from Maze13 is Maze12. South of Maze13 is Maze10. West of Maze13 is Maze11.
Maze14 is a dark room. The printed name of Maze14 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze14 is in the Underground.
West of Maze14 is Maze15. Northwest of Maze14 is Maze14. Northeast of Maze14 is Maze7. South of Maze14 is Maze7.
Maze15 is a dark room. The printed name of Maze15 is "Maze". "This is part of a maze of twisty little passages, all alike."
Maze15 is in the Underground.
West of Maze15 is Maze14. South of Maze15 is Maze7. Southeast of Maze15 is Cyclops-Room.


-}