module Yaifl.Zork.Troll where

{-

Chapter 4 - Troll NPC
The troll is a person in Troll-Room. "[if the troll-unconscious is true]An unconscious troll is sprawled on the floor. All passages out of the room are open[otherwise if the troll carries the bloody axe]A nasty-looking troll, brandishing a bloody axe, blocks all passages out of the room[otherwise]A troll is here[end if]."
Understand "troll" and "nasty" as the troll.
The description of the troll is "[if the troll is defeated]The troll is dead.[otherwise if the troll-unconscious is true]An unconscious troll is sprawled on the floor. All passages out of the room are open.[otherwise if the troll carries the bloody axe]A nasty-looking troll, brandishing a bloody axe, blocks all passages out of the room.[otherwise]A troll is here.[end if]".
The troll-strength is a number that varies. The troll-strength is 2.
The troll-unconscious is a truth state that varies. The troll-unconscious is false.
The troll-recovery-chance is a number that varies. The troll-recovery-chance is 0.
The bloody axe is carried by the troll. "There is a bloody axe here."
Understand "axe" and "ax" and "bloody" as the bloody axe.
The bloody axe is a weapon.
Instead of taking the bloody axe when the troll is not defeated and the troll carries the bloody axe:
	say "The troll swings it out of your reach."
Instead of attacking the troll:
	if the troll is not in the location of the player:
		say "There is no troll here." instead;
	if the troll is defeated:
		say "The troll is already dead.";
	otherwise if the troll-unconscious is true:
		say "The unconscious troll cannot defend himself: He dies.";
		say "[line break][sinister-black-fog for the troll]";
		now the troll is defeated;
		now the troll-flag is true;
		now the troll-unconscious is false;
		now the troll-recovery-chance is 0;
		if the troll carries the bloody axe:
			now the bloody axe is in Troll-Room;
		remove the troll from play;
	otherwise:
		let W be a random weapon carried by the player;
		if W is nothing:
			say "Trying to attack the troll with your bare hands is suicidal.";
		otherwise:
			now the melee-weapon is W;
			now the melee-target is the troll;
			let hit-chance be a random number between 1 and 10;
			if hit-chance is at least 4:
				let outcome be a random number between 1 and 3;
				if outcome is 1:
					print hero melee for "unconscious";
					now the troll-unconscious is true;
					now the troll-recovery-chance is 0;
					now the troll-flag is true;
					if the troll carries the bloody axe:
						now the bloody axe is in Troll-Room;
				otherwise:
					decrease the troll-strength by 1;
					if the troll-strength is at most 0:
						print hero melee for "kill";
						say "[line break][sinister-black-fog for the troll]";
						now the troll is defeated;
						now the troll-flag is true;
						if the troll carries the bloody axe:
							now the bloody axe is in Troll-Room;
						remove the troll from play;
					otherwise:
						print hero melee for "light-wound";
			otherwise:
				print hero melee for "miss".
Instead of telling the troll about something:
	say "The troll isn't much of a conversationalist."
Instead of giving something to the troll:
	if the noun is the bloody axe:
		say "The troll scratches his head in confusion, then takes the axe.";
		now the troll carries the bloody axe;
	otherwise:
		say "The troll, who is not overly proud, graciously accepts the gift and eats it hungrily.";
		remove the noun from play.
Every turn when the troll is not defeated and the troll-unconscious is false and the troll is in Troll-Room and the player is in Troll-Room (this is the troll attacks rule):
	let W be a random weapon carried by the player;
	if W is not nothing:
		now the melee-weapon is W;
	if a random chance of 1 in 3 succeeds:
		if W is not nothing:
			print troll melee for "miss";
		otherwise:
			print troll melee for "kill";
			die saying "It appears that that last blow was too much for you. I'm afraid you are dead."
Every turn when the troll-unconscious is true and the troll is in Troll-Room (this is the troll recovery rule):
	if the troll-recovery-chance > 0:
		let roll be a random number between 1 and 100;
		if roll is at most the troll-recovery-chance:
			now the troll-unconscious is false;
			now the troll-recovery-chance is 0;
			now the troll-flag is false;
			if the bloody axe is in Troll-Room:
				now the troll carries the bloody axe;
			if the player is in Troll-Room:
				say "The troll stirs, quickly resuming a fighting stance.";
			rule succeeds;
	increase the troll-recovery-chance by 25.
Instead of taking the troll:
	say "The troll spits in your face, grunting [quotation mark]Better luck next time[quotation mark] in a rather barbarous accent."
Instead of pushing the troll:
	say "The troll laughs at your puny gesture."
Instead of listening to the troll:
	say "Every so often the troll says something, probably uncomplimentary, in his guttural tongue."
Instead of throwing something at the troll:
	if the troll is not in the location of the player:
		say "There is no troll here." instead;
	if the noun is a weapon:
		if a random chance of 1 in 5 succeeds:
			say "The troll, who is remarkably coordinated, catches the [noun] and, not having the most discriminating tastes, gleefully eats it. Poor troll, he dies from an internal hemorrhage and his carcass disappears in a sinister black fog.";
			remove the noun from play;
			now the troll is defeated;
			now the troll-flag is true;
			now the troll-unconscious is false;
			now the troll-recovery-chance is 0;
			if the troll carries the bloody axe:
				now the bloody axe is in Troll-Room;
			remove the troll from play;
		otherwise:
			say "The troll, who is remarkably coordinated, catches the [noun] and, being for the moment sated, throws it back. Fortunately, the troll has poor control, and the [noun] falls to the floor. He does not look pleased.";
			now the noun is in the location of the player;
	otherwise:
		say "The troll, who is remarkably coordinated, catches the [noun] and, not having the most discriminating tastes, gleefully eats it.";
		remove the noun from play.
Instead of answering the troll that "hello":
	if the troll is defeated:
		say "Unfortunately, the troll can[apostrophe]t hear you."
-}