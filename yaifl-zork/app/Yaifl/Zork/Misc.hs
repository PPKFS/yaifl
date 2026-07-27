module Yaifl.Zork.Misc where

{-Chapter 2 - Sword Glow
The sword-glow-level is a number that varies. The sword-glow-level is 0.
To decide whether (V - a person) is a nearby villain:
	if V is in the location of the player, yes;
	no.
To decide whether a villain lurks adjacent:
	repeat with D running through directions:
		let R be the room D from the location of the player;
		if R is a room:
			if the troll is not defeated and the troll is in R, yes;
			if the thief is not defeated and the thief is in R, yes;
			if the cyclops is in R, yes;
	no.
Every turn when the player carries the sword (this is the sword glow rule):
	let danger-level be 0;
	if the troll is not defeated and the troll is a nearby villain:
		now danger-level is 2;
	if the thief is not defeated and the thief is a nearby villain:
		now danger-level is 2;
	if the cyclops is a nearby villain:
		now danger-level is 2;
	if danger-level is 0 and a villain lurks adjacent:
		now danger-level is 1;
	if danger-level is not the sword-glow-level:
		play the sound of sword-sfx as sfx;
		if danger-level is 0:
			say "Your sword is no longer glowing.";
		otherwise if danger-level is 1:
			say "Your sword is glowing with a faint blue glow.";
		otherwise:
			say "Your sword has begun to glow very brightly.";
		now the sword-glow-level is danger-level.
Instead of examining the sword:
	if the sword-glow-level is 1:
		say "Your sword is glowing with a faint blue glow.";
	otherwise if the sword-glow-level is 2:
		say "Your sword is glowing very brightly.";
	otherwise:
		continue the action.
Part 5 - Miscellaneous Actions and Rules
Chapter 1 - Hello Sailor
The hello-sailor-count is a number that varies. The hello-sailor-count is 0.
Hello-sailoring is an action applying to nothing. Understand "hello sailor" as hello-sailoring.
Carry out hello-sailoring:
	increase the hello-sailor-count by 1;
	if the remainder after dividing the hello-sailor-count by 20 is 0:
		say "You seem to be repeating yourself.";
	otherwise if the remainder after dividing the hello-sailor-count by 10 is 0:
		say "I think that phrase is getting a bit worn out.";
	otherwise:
		say "Nothing happens here."
Chapter 2 - Pray
Praying is an action applying to nothing. Understand "pray" as praying.
Carry out praying:
	if the player-is-dead is true:
		if the player is in South Temple:
			now the player-is-dead is false;
			now the always-lit-mode is false;
			if the troll is in Troll-Room:
				now the troll-flag is false;
				now the troll-unconscious is false;
				now the troll-recovery-chance is 0;
				if the bloody axe is in Troll-Room:
					now the troll carries the bloody axe;
			now the spirit-glow is nowhere;
			say "From the distance the sound of a lone trumpet is heard. The room becomes very bright and you feel disembodied. In a moment, the brightness fades and you find yourself rising as if from a long sleep, deep in the woods. In the distance you can faintly hear a songbird and the sounds of the forest.";
			move the player to Forest1;
		otherwise:
			say "Your prayers are not heard.";
	otherwise if the player is in South Temple:
		move the player to Forest1;
	otherwise:
		say "If you pray enough, your prayers may be answered."
Chapter 4 - Diagnose
Diagnosing is an action out of world. Understand "diagnose" as diagnosing.
Carry out diagnosing:
	if the player-is-dead is true:
		say "You are dead.";
		stop the action;
	say "You are in perfect health.[line break]";
	say "You can survive several wounds.[line break]";
	if the player-deaths > 0:
		say "You have been killed ";
		if the player-deaths is 1:
			say "once";
		otherwise:
			say "twice";
		say ".[line break]"
Chapter 4a - Self-Referential Actions
Instead of telling yourself about something:
	say "Talking to yourself is said to be a sign of impending mental collapse."
Instead of eating yourself:
	say "Auto-cannibalism is not the answer."
Instead of attacking yourself:
	let W be a random weapon carried by the player;
	if W is not nothing:
		die saying "If you insist.... Poof, you're dead!";
	otherwise:
		say "Suicide is not the answer."
Instead of pushing yourself:
	say "Why don't you just walk like normal people?"
Instead of taking yourself:
	say "How romantic!"
Instead of examining yourself:
	if the player is in Mirror Room 1 or the player is in Mirror Room 2:
		say "Your image in the mirror looks tired.";
	otherwise:
		say "That's difficult unless your eyes are prehensile."
Making is an action applying to one thing. Understand "make [something]" as making.
Carry out making: say "You can't make that."
Instead of making yourself:
	say "Only you can do that."
Chapter 5 - Rusty Knife Curse
Instead of taking the rusty knife when the player carries the sword:
	say "As you touch the rusty knife, your sword gives a single pulse of blinding blue light.";
	remove the rusty knife from play.
Instead of attacking a person when the player carries the rusty knife:
	say "As the knife approaches its victim, your mind is submerged by an overmastering will. Slowly, your hand turns, until the rusty blade is an inch from your neck. The knife seems to sing as it savagely slits your throat.";
	die saying ""
Every turn when the player carries the rusty knife and the player carries the sword (this is the rusty knife curse rule):
	say "As the rust of the knife reaches the sword, they react violently, and the rusty knife disintegrates.";
	remove the rusty knife from play.
Chapter 6 - Chimney Passage
Instead of going up in Studio:
	let items-carried be 0;
	repeat with item running through things carried by the player:
		increase items-carried by 1;
	if items-carried is 0:
		say "Going up empty-handed is a bad idea.";
	otherwise if items-carried > 2:
		say "You can't get up there with what you're carrying.";
	otherwise if the player carries the brass lantern and items-carried <= 2:
		if the kitchen-visited is false:
			now the kitchen-visited is true;
			increase the score by 10;
		move the player to Kitchen;
	otherwise:
		say "You can't get up there with what you're carrying."
Chapter 7 - Room Visit Points
The cellar-visited is a truth state that varies. The cellar-visited is false.
The kitchen-visited is a truth state that varies. The kitchen-visited is false.
The east-west-visited is a truth state that varies. The east-west-visited is false.
The treasure-room-visited is a truth state that varies. The treasure-room-visited is false.
After going to Cellar when the cellar-visited is false:
	now the cellar-visited is true;
	increase the score by 25;
	continue the action.
After going to Kitchen when the kitchen-visited is false:
	now the kitchen-visited is true;
	increase the score by 10;
	continue the action.
After going to East-West Passage when the east-west-visited is false:
	now the east-west-visited is true;
	increase the score by 5;
	continue the action.
After going to Treasure Room when the treasure-room-visited is false:
	now the treasure-room-visited is true;
	increase the score by 25;
	continue the action.
The light-shaft-bonus is a truth state that varies. The light-shaft-bonus is false.
After going to Drafty Room when the light-shaft-bonus is false and not in darkness:
	now the light-shaft-bonus is true;
	increase the score by 13;
	continue the action.
Chapter 8 - Ancient Map
The ancient map is in the trophy case. The ancient map is zil-invisible.
Understand "parchment" and "map" and "antique" and "old" and "ancient" as the ancient map.
The description of the ancient map is "The map shows a forest with three clearings. The largest clearing contains a house. Three paths leave the large clearing. One of these paths, leading southwest, is marked 'To Stone Barrow'."
Chapter 9 - Lurking Grue
The lurking grue is a backdrop. The lurking grue is everywhere.
Understand "grue" and "lurking" and "sinister" and "hungry" and "silent" as the lurking grue.
The description of the lurking grue is "The grue is a sinister, lurking presence in the dark places of the earth. Its favorite diet is adventurers, but its insatiable appetite is tempered by its fear of light. No grue has ever been seen by the light of day, and few have survived its fearsome jaws to tell the tale."
Instead of finding the lurking grue:
	say "There is no grue here, but I'm sure there is at least one lurking in the darkness nearby. I wouldn't let my light go out if I were you!"
Instead of listening to the lurking grue:
	say "It makes no sound but is always lurking in the darkness nearby."
Chapter 9a - Global Backdrops
The global-hands is a backdrop. The global-hands is everywhere.
The printed name of the global-hands is "pair of hands".
Understand "hands" and "hand" and "pair" as the global-hands.
Understand "bare" as the global-hands.
The description of the global-hands is "You have two normal-looking hands."
The global-lungs is a backdrop. The global-lungs is everywhere.
The printed name of the global-lungs is "blast of air".
Understand "lungs" and "air" and "mouth" and "breath" as the global-lungs.
The description of the global-lungs is "You have normal-looking lungs."
The global-stairs is a backdrop. The global-stairs is everywhere.
The printed name of the global-stairs is "stairs".
Understand "stairs" and "staircase" and "stairway" and "steps" as the global-stairs.
The description of the global-stairs is "The stairs lead up and down."
Instead of entering the global-stairs:
	say "You should say whether you want to go up or down."
Instead of climbing the global-stairs:
	say "You should say whether you want to go up or down."
The global-path is a backdrop. The global-path is in House Exterior, Forest Area, and Underground.
The printed name of the global-path is "path".
Understand "path" and "trail" as the global-path.
The description of the global-path is "The path leads in several directions."
Instead of taking the global-path:
	say "You must specify a direction to go."
Instead of finding the global-path:
	say "I can't help you there...."
Following is an action applying to one thing. Understand "follow [something]" as following.
Carry out following: say "You can't follow that."
Instead of following the global-path:
	say "You must specify a direction to go."
Instead of digging the global-path:
	say "Not a chance."
The global-teeth is a backdrop. The global-teeth is everywhere.
The printed name of the global-teeth is "teeth".
Understand "teeth" and "tooth" as the global-teeth.
The description of the global-teeth is "You have the usual complement of teeth."
Instead of brushing the global-teeth: say "Dental hygiene is highly recommended, but I'm not sure what you want to brush them with."
The global-zorkmid is a backdrop. The global-zorkmid is everywhere.
The printed name of the global-zorkmid is "zorkmid".
Understand "zorkmid" and "zorkmids" and "currency" as the global-zorkmid.
The description of the global-zorkmid is "The zorkmid is the unit of currency of the Great Underground Empire."
Instead of finding the global-zorkmid:
	say "The best way to find zorkmids is to go out and look for them."
Chapter 10 - Additional Game Verbs
Understand "xyzzy" as a mistake ("A hollow voice says [quotation mark]Fool.[quotation mark]").
Understand "plugh" as a mistake ("A hollow voice says [quotation mark]Fool.[quotation mark]").
Counting is an action applying to one thing. Understand "count [something]" as counting.
Carry out counting: say "You have lost your mind."
Counting-blessings is an action applying to nothing. Understand "count blessings" and "count my blessings" as counting-blessings.
Carry out counting-blessings: say "Well, for one, you are playing Zork..."
Instead of counting the pile of leaves: say "There are 69,105 leaves here."
Instead of counting the pair of candles: say "Let[apostrophe]s see, how many objects in a pair? Don[apostrophe]t tell me, I[apostrophe]ll get it."
Instead of counting the matchbook:
	let cnt be the match-count minus 1;
	say "You have ";
	if cnt is less than 1:
		say "no";
	otherwise:
		say "[cnt]";
	if cnt is 1:
		say " match.";
	otherwise:
		say " matches."
Instead of opening the matchbook:
	let cnt be the match-count minus 1;
	say "You have ";
	if cnt is less than 1:
		say "no";
	otherwise:
		say "[cnt]";
	if cnt is 1:
		say " match.";
	otherwise:
		say " matches."
Zorking is an action applying to nothing. Understand "zork" as zorking.
Carry out zorking: say "At your service!"
Frobozzing is an action applying to nothing. Understand "frobozz" as frobozzing.
Carry out frobozzing: say "The FROBOZZ Corporation created, owns, and operates this dungeon."
Winning is an action applying to nothing. Understand "win" as winning.
Carry out winning: say "Naturally!"
Yelling is an action applying to nothing. Understand "yell" and "scream" and "shout" as yelling.
Carry out yelling: say "Aaaarrrrgggghhhh!"
Repenting is an action applying to nothing. Understand "repent" as repenting.
Carry out repenting: say "It could very well be too late!"
Raping is an action applying to nothing. Understand "rape" as raping.
Carry out raping: say "What a (ahem!) strange idea."
Instead of turning something: say "This has no effect."
Instead of waiting: say "Time passes..."
Swimming is an action applying to nothing. Understand "swim" as swimming.
Instead of swimming: say "Swimming isn't usually allowed in the dungeon."
Understand "look behind [something]" as looking under.
Instead of kissing someone: say "I'd sooner kiss a pig."
Instead of smelling something: say "It smells like a [noun]."
Instead of listening to something: say "The [noun] makes no sound."
Chapter 10a - Burn Action
Burning it with is an action applying to two things. Understand "burn [something] with [something]" and "light [something] with [something]" as burning it with.
Instead of burning the pair of candles with the matchbook:
	if the match-lit is true:
		try lighting-candles the pair of candles instead;
	say "You should light the match first."
Instead of burning the pair of candles with the torch:
	say "The heat from the torch is so intense that the candles are vaporized.";
	remove the pair of candles from play.
Instead of burning the pair of candles with something:
	say "You have to light them with something that[apostrophe]s burning, you know."
Carry out burning it with:
	say "You can[apostrophe]t burn that."
Chapter 10b - Generic Verb Handlers
Section 1 - Simple One-Response Verbs
Answering-nobody is an action applying to nothing. Understand "answer" as answering-nobody.
Carry out answering-nobody: say "Nobody seems to be awaiting your answer."
Going-back is an action applying to nothing. Understand "back" and "go back" as going-back.
Carry out going-back: say "Sorry, my memory is poor. Please give a direction."
Blasting is an action applying to nothing. Understand "blast" as blasting.
Carry out blasting: say "You can't blast anything by using words."
Brushing is an action applying to one thing. Understand "brush [something]" as brushing.
Carry out brushing: say "If you wish, but heaven only knows why."
Brushing it with is an action applying to two things. Understand "brush [something] with [something]" as brushing it with.
Instead of brushing the global-teeth with the viscous material:
	die saying "Well, you seem to have been brushing your teeth with some sort of glue. As a result, your mouth gets glued together (with your nose) and you die of respiratory failure."
Instead of brushing the global-teeth with something:
	say "A nice idea, but with a [second noun]?"
Carry out brushing something with something: say "If you wish, but heaven only knows why."
Bugging is an action applying to nothing. Understand "bug" as bugging.
Carry out bugging: say "Bug? Not in a flawless program like this! (Cough, cough)."
Chomping is an action applying to one thing. Understand "bite [something]" and "chomp [something]" as chomping.
Carry out chomping: say "Preposterous!"
Instead of climbing something: say "You can't do that!"
Hatching is an action applying to one thing. Understand "hatch [something]" as hatching.
Carry out hatching: say "Bizarre!"
Instead of cutting something: say "Strange concept, cutting the [noun]...."
Cutting-with is an action applying to two things. Understand "cut [something] with [something]" and "slice [something] with [something]" as cutting-with.
Instead of cutting-with when the noun is a person:
	try attacking the noun.
Instead of cutting-with when the player is in the noun:
	say "Not a bright idea, especially since you[apostrophe]re in it."
Instead of cutting-with when the second noun is not a weapon:
	say "The [quotation mark]cutting edge[quotation mark] of a [second noun] is hardly adequate."
Instead of cutting-with:
	say "Strange concept, cutting the [noun]...."
Leaning-on is an action applying to one thing. Understand "lean on [something]" and "lean against [something]" as leaning-on.
Carry out leaning-on: say "Getting tired?"
Locking is an action applying to one thing. Understand "lock [something]" as locking.
Carry out locking: say "It doesn't seem to work."
Melting is an action applying to one thing. Understand "melt [something]" as melting.
Carry out melting: say "It's not clear that a [noun] can be melted."
Mumbling is an action applying to nothing. Understand "mumble" as mumbling.
Carry out mumbling: say "You'll have to speak up if you expect me to hear you!"
Oiling is an action applying to one thing. Understand "oil [something]" and "lubricate [something]" and "grease [something]" as oiling.
Carry out oiling: say "You probably put spinach in your gas tank, too."
Instead of putting the viscous material on something:
	if the second noun is the leak:
		try plugging the leak with the viscous material;
	otherwise:
		say "The all-purpose gunk isn[apostrophe]t a lubricant."
Lock-picking is an action applying to one thing. Understand "pick [something]" as lock-picking.
Carry out lock-picking: say "You can[apostrophe]t pick that."
Instead of lock-picking the grate: say "You can[apostrophe]t pick the lock."
Plugging is an action applying to one thing. Understand "plug [something]" as plugging.
Carry out plugging: say "This has no effect."
Putting-under is an action applying to two things. Understand "put [something] under [something]" as putting-under.
Carry out putting-under: say "You can't do that."
Hiding-behind is an action applying to two things. Understand "put [something] behind [something]" and "hide [something] behind [something]" as hiding-behind.
Carry out hiding-behind: say "That hiding place is too obvious."
Replying is an action applying to one thing. Understand "reply to [something]" and "reply [something]" as replying.
Carry out replying: say "It is hardly likely that the [noun] is interested."
Instead of searching a door:
	if the noun is open:
		say "The [noun] is open, but I can[apostrophe]t tell what[apostrophe]s beyond it.";
	otherwise:
		say "The [noun] is closed."
Instead of searching something that is not a container: say "You find nothing unusual."
Spinning is an action applying to one thing. Understand "spin [something]" as spinning.
Carry out spinning: say "You can't spin that!"
Stabbing is an action applying to one thing. Understand "stab [something]" as stabbing.
Carry out stabbing:
	let W be a random weapon carried by the player;
	if W is not nothing:
		try attacking the noun;
	otherwise:
		say "No doubt you propose to stab the [noun] with your pinky?"
Standing-up is an action applying to nothing. Understand "stand" and "stand up" as standing-up.
Carry out standing-up: say "You are already standing, I think."
Staying is an action applying to nothing. Understand "stay" as staying.
Carry out staying: say "You will be lost without me!"
Striking is an action applying to one thing. Understand "strike [something]" as striking.
Carry out striking:
	if the noun is a person:
		say "Since you aren't versed in hand-to-hand combat, you'd better attack the [noun] with a weapon.";
	otherwise:
		try switching on the noun.
Instead of swinging something: say "Whoosh!"
Throwing-off is an action applying to one thing. Understand "throw [something] off" as throwing-off.
Carry out throwing-off: say "You can't throw anything off of that!"
Tying-up is an action applying to two things. Understand "tie up [something] with [something]" and "tie [something] up with [something]" as tying-up.
Carry out tying-up: say "You could certainly never tie it with that!"
Treasuring is an action applying to nothing. Understand "treasure" as treasuring.
Carry out treasuring: say "Nothing happens."
Untieing is an action applying to one thing. Understand "untie [something]" as untieing.
Carry out untieing: say "This cannot be tied, so it cannot be untied!"
Walking-around is an action applying to nothing. Understand "walk around" as walking-around.
Carry out walking-around: say "Use compass directions for movement."
Instead of wearing something: say "You can't wear the [noun]."
Wishing is an action applying to nothing. Understand "wish" as wishing.
Carry out wishing: say "With luck, your wish will come true."
Drink-froming is an action applying to one thing. Understand "drink from [something]" as drink-froming.
Carry out drink-froming: say "How peculiar!"
Section 2 - Multi-Branch Verb Handlers
Cursing is an action applying to nothing. Understand "curse" and "damn" and "shit" and "fuck" as cursing.
Carry out cursing: say "Such language in a high-class establishment like this!"
Cursing-at is an action applying to one thing. Understand "curse [something]" and "damn [something]" as cursing-at.
Carry out cursing-at:
	if the noun is a person:
		say "Insults of this nature won't help you.";
	otherwise:
		say "What a loony!"
Commanding is an action applying to one thing. Understand "command [something]" as commanding.
Carry out commanding:
	if the noun is a person:
		say "The [noun] pays no attention.";
	otherwise:
		say "You cannot talk to that!"
Knocking-on is an action applying to one thing. Understand "knock on [something]" and "knock [something]" as knocking-on.
Carry out knocking-on:
	if the noun is a door:
		say "Nobody's home.";
	otherwise:
		say "Why knock on a [noun]?"
Instead of pushing something:
	if the player carries the noun:
		say "You aren[apostrophe]t an accomplished enough juggler.";
	otherwise if the noun is fixed in place:
		say "You can[apostrophe]t move the [noun].";
	otherwise:
		say "Moving the [noun] reveals nothing."
Instead of squeezing a person: say "The [noun] does not understand this."
Instead of squeezing something: say "How singularly useless."
Understand "spray [something]" as squeezing.
Sending is an action applying to one thing. Understand "send for [something]" and "send [something]" as sending.
Carry out sending:
	if the noun is a person:
		say "Why would you send for the [noun]?";
	otherwise:
		say "That doesn't make sends."
Walking-to is an action applying to one visible thing. Understand "walk to [something]" and "go to [something]" as walking-to.
Carry out walking-to:
	if the player can see the noun:
		say "It's here!";
	otherwise:
		say "You should supply a direction!"
Saying-something is an action applying to one topic. Understand "say [text]" as saying-something.
Carry out saying-something: say "Talking to yourself is a sign of impending mental collapse."
Section 3 - Hello and Greetings
Helloing is an action applying to one visible thing. Understand "hello [something]" and "greet [something]" and "hi [something]" as helloing.
Carry out helloing:
	if the noun is a person:
		say "The [noun] bows his head to you in greeting.";
	otherwise:
		say "It's a well known fact that only schizophrenics say [quotation mark]Hello[quotation mark] to a [noun]."
Helloing-nobody is an action applying to nothing. Understand "hello" and "hi" and "greetings" as helloing-nobody.
Carry out helloing-nobody:
	let R be a random number between 1 and 4;
	if R is 1:
		say "Hello.";
	otherwise if R is 2:
		say "Good day.";
	otherwise if R is 3:
		say "Nice weather we've been having lately.";
	otherwise:
		say "Goodbye."
Section 4 - Shaking
Shaking is an action applying to one thing. Understand "shake [something]" as shaking.
Instead of shaking the glass bottle:
	if the glass bottle is open and the quantity of water is in the glass bottle:
		remove the quantity of water from play;
		say "The water spills to the floor and evaporates.";
	otherwise:
		say "Shaken."
Carry out shaking:
	if the noun is a person:
		say "This seems to have no effect.";
	otherwise if the noun is fixed in place:
		say "You can't take it; thus, you can't shake it!";
	otherwise if the noun is an open container:
		let stuff-found be false;
		repeat with item running through things in the noun:
			now stuff-found is true;
			now item is in the location of the player;
		if stuff-found is true:
			say "The contents of the [noun] spill to the ground.";
		otherwise:
			say "Shaken.";
	otherwise if the noun is a closed container:
		let stuff-found be false;
		repeat with item running through things in the noun:
			now stuff-found is true;
		if stuff-found is true:
			say "It sounds like there is something inside the [noun].";
		otherwise:
			say "The [noun] sounds empty.";
	otherwise:
		say "Shaken."
[ZIL V-KICK → I7 kicking]
Kicking is an action applying to one thing. Understand "kick [something]" as kicking.
Carry out kicking: say "Kicking the [noun] [ho-hum]"
Section 4c - FIND verb
Locating is an action applying to one visible thing. Understand "find [something]" and "where is [something]" as locating.
Carry out locating:
	if the noun is the global-hands or the noun is the global-lungs:
		say "Within six feet of your head, assuming you haven[apostrophe]t left that somewhere.";
	otherwise if the noun is the player:
		say "You[apostrophe]re around here somewhere...";
	otherwise if the player carries the noun:
		say "You have it.";
	otherwise if the noun is a person:
		if the player can see the noun:
			say "Right in front of you, perhaps?";
		otherwise:
			say "Beats me.";
	otherwise if the player can see the noun:
		say "You find it.";
	otherwise:
		say "Beats me."
Section 4d - SWIM verb
Pool-swimming is an action applying to nothing. Understand "swim" and "dive" as pool-swimming.
Carry out pool-swimming:
	if the global-water is in the location of the player:
		say "Swimming isn[apostrophe]t usually allowed in the dungeon.";
	otherwise:
		say "Go jump in a lake!"
Section 4e - Additional LOOK actions
Instead of looking under something: say "There is nothing but dust there."
Looking-behind is an action applying to one thing. Understand "look behind [something]" as looking-behind.
Carry out looking-behind: say "There is nothing behind the [noun]."
Looking-on is an action applying to one thing. Understand "look on [something]" as looking-on.
Carry out looking-on:
	if the noun is a supporter:
		try searching the noun;
	otherwise:
		say "Look on a [noun]???"
Section 5 - Throwing Overrides
Before throwing a backdrop at something:
	say "You can't throw that!" instead.
Instead of throwing something at yourself:
	say "A terrific throw! The [noun] hits you squarely in the head. Normally, this wouldn[apostrophe]t do much damage, but by incredible mischance, you fall over backwards trying to duck, and break your neck, justice being swift and merciful in the Great Underground Empire.";
	die saying ""
Instead of throwing something at a person:
	if the second noun is the thief and the thief-unconscious is true:
		say "The thief is unconscious.";
		now the noun is in the location of the player instead;
	if the second noun is the thief and the noun is a weapon:
		if a random chance of 1 in 10 succeeds:
			say "You evidently frightened the robber, though you didn't hit him. He flees.";
			let new-dest be a random dark room that is in the Underground;
			if new-dest is a room:
				move the thief to new-dest;
		otherwise:
			say "You missed. The thief makes no attempt to take the knife, though it would be a fine addition to the collection in his bag. He does seem angered by your attempt.";
		now the noun is in the location of the player;
	otherwise:
		say "The [second noun] ducks as the [noun] flies by and crashes to the ground.";
		now the noun is in the location of the player.
Instead of throwing something at something:
	say "Thrown.";
	now the noun is in the location of the player.
Section 6 - Jump / Leap Overrides
The wheeeee-count is a number that varies. The wheeeee-count is 0.
Instead of jumping:
	increase the wheeeee-count by 1;
	let R be the remainder after dividing the wheeeee-count by 4;
	if R is 1:
		say "Very good. Now you can go to the second grade.";
	otherwise if R is 2:
		say "Are you enjoying yourself?";
	otherwise if R is 3:
		say "Wheeeeeeeeee!!!!!";
	otherwise:
		say "Do you expect me to applaud?"
Section 7 - Through / Enter Overrides
Going-through is an action applying to one thing. Understand "go through [something]" and "walk through [something]" as going-through.
Carry out going-through:
	if the noun is a door:
		try entering the noun;
	otherwise if the player carries the noun:
		say "That would involve quite a contortion!";
	otherwise:
		say "You hit your head against the [noun] as you attempt this feat."
Section 8 - Eat Overrides
Instead of eating something:
	if the noun is edible:
		continue the action;
	otherwise:
		say "I don't think that the [noun] would agree with you."
Section 9 - Read Defaults
Instead of reading something:
	if in darkness:
		say "It is impossible to read in the dark.";
	otherwise:
		say "How does one read a [noun]?"
Section 10 - Alarm / Wake
Alarming is an action applying to one thing. Understand "alarm [something]" and "wake [something]" and "wake up [something]" as alarming.
Carry out alarming:
	if the noun is a person:
		if the noun is the troll and the troll is defeated:
			say "The troll is rudely awakened.";
		otherwise if the noun is the thief and the thief is defeated:
			say "The thief is rudely awakened.";
		otherwise if the noun is the cyclops and the cyclops-asleep is true:
			say "The cyclops is rudely awakened.";
		otherwise:
			say "The [noun] is wide awake, or haven[apostrophe]t you noticed...";
	otherwise:
		say "The [noun] isn[apostrophe]t sleeping."
Section 11 - Play
Playing is an action applying to one thing. Understand "play [something]" as playing.
Carry out playing:
	if the noun is a person:
		die saying "You become so engrossed in the role of the [noun] that you kill yourself, just as he might have done!";
	otherwise:
		say "That's silly!"
Section 12 - Dig Defaults
Instead of digging something:
	if the player does not carry the shovel:
		say "Digging with your hands is silly.";
	otherwise:
		say "There's no reason to be digging here."
Section 13 - Attack Defaults
Instead of attacking something:
	if the noun is not a person:
		say "I[apostrophe]ve known strange people, but fighting a [noun]?";
	otherwise:
		let W be a random weapon carried by the player;
		if W is nothing:
			say "Strangle him with your bare hands?";
		otherwise:
			continue the action.
Section 14 - Climb Defaults
Instead of climbing the granite-wall: say "Climbing the walls is to no avail."
Section 15 - Burn Defaults
Instead of burning something:
	if the player does not carry the matchbook and the player does not carry the torch:
		say "You should say what to burn it with.";
	otherwise:
		say "You can[apostrophe]t burn a [noun]."
Section 16 - Give Defaults
Instead of giving something to something:
	if the second noun is not a person:
		say "You can[apostrophe]t give a [noun] to a [second noun]!";
	otherwise:
		say "The [second noun] refuses it politely."
Section 17 - Tell Defaults
Instead of telling someone about something:
	say "The [noun] pauses for a moment, perhaps thinking that you should reread the manual."
Section 18 - Tie Defaults
Instead of tying something to yourself: say "You can[apostrophe]t tie anything to yourself."
Section 19 - Destroy / Mung
Destroying is an action applying to one thing. Understand "destroy [something]" and "mung [something]" and "damage [something]" as destroying.
Instead of destroying a person:
	try attacking the noun.
Instead of destroying something:
	say "Nice try."
Destroying-with is an action applying to two things. Understand "destroy [something] with [something]" and "mung [something] with [something]" as destroying-with.
Instead of destroying-with a person:
	try attacking the noun.
Instead of destroying-with when the second noun is a weapon:
	try attacking the noun.
Instead of destroying-with:
	say "Trying to destroy the [noun] with [if the player carries the second noun]a [second noun][otherwise]your bare hands[end if] is futile."
Section 20 - Overboard
Throwing-overboard is an action applying to one thing. Understand "throw [something] overboard" as throwing-overboard.
Instead of throwing-overboard when the player is in the magic boat:
	say "Ahoy -- [noun] overboard!";
	now the noun is in the location of the player.
Instead of throwing-overboard:
	say "Huh?"
Section 21 - Leaping Over
Leaping-over is an action applying to one thing. Understand "jump over [something]" and "leap over [something]" and "jump across [something]" as leaping-over.
Instead of leaping-over a person:
	say "The [noun] is too big to jump over."
Instead of leaping-over:
	continue the action.
Section 22 - Disembark Messages
Instead of exiting when the player is in the magic boat:
	let here be the location of the player;
	if here is River1 or here is River2 or here is River3 or here is River4 or here is River5:
		say "You realize that getting out here would be fatal.";
	otherwise if here is Reservoir or here is In-Stream:
		say "You realize that getting out here would be fatal.";
	otherwise:
		say "You are on your own feet again.";
		move the player to here, without printing a room description.
Chapter 12 - Buoy and Emerald
After taking the red buoy:
	say "Taken.[line break]You notice something funny about the feel of the buoy.";
	continue the action.
The red buoy is in River4. "There is a red buoy here (probably a warning)."
Understand "buoy" and "red" as the red buoy.
The red buoy is a closed openable container. The carrying capacity of the red buoy is 3.
Report opening the red buoy when the large emerald is in the red buoy:
	say "Opening the red buoy reveals a large emerald." instead.
The large emerald is in the red buoy.
Understand "emerald" and "large" as the large emerald.
The treasure-value of the large emerald is 10.
The point-value of the large emerald is 5.
Chapter 13 - Trunk of Jewels
The trunk of jewels is in Reservoir. "Lying half buried in the mud is an old trunk, bulging with jewels." The trunk of jewels is zil-invisible.
Understand "trunk" and "chest" and "jewels" and "old" as the trunk of jewels.
The treasure-value of the trunk of jewels is 5.
The point-value of the trunk of jewels is 15.
The description of the trunk of jewels is "There is an old trunk here, bulging with assorted jewels."
Instead of opening the trunk of jewels: say "The jewels are safely inside; there[apostrophe]s no need to do that."
Instead of closing the trunk of jewels: say "The jewels are safely inside; there[apostrophe]s no need to do that."
Instead of inserting something into the trunk of jewels: say "Don[apostrophe]t be silly. It wouldn[apostrophe]t be a trunk of jewels anymore."
Instead of searching the trunk of jewels: say "There are lots of jewels in there."
After going to Reservoir:
	if the low-tide is true and the trunk of jewels is zil-invisible:
		now the trunk of jewels is zil-visible;
		say "Lying half buried in the mud is an old trunk, bulging with jewels.[line break]";
	continue the action.
Chapter 14 - Gate/Bolt Interaction
Instead of turning the bolt:
	if the player does not carry the wrench:
		say "The bolt won't turn with your best effort.";
	otherwise if the gate-flag is false:
		say "The bolt won't turn with your best effort.";
	otherwise if the gates-open is true:
		now the gates-open is false;
		now the reservoir-fill-timer is 8;
		say "The sluice gates close and water starts to collect behind the dam.";
	otherwise:
		now the gates-open is true;
		now the reservoir-empty-timer is 8;
		play the sound of flood-sfx as sfx;
		say "The sluice gates open and water pours through the dam."
The reservoir-fill-timer is a number that varies. The reservoir-fill-timer is 0.
The reservoir-empty-timer is a number that varies. The reservoir-empty-timer is 0.
Every turn when the reservoir-empty-timer > 0 (this is the reservoir emptying rule):
	decrease the reservoir-empty-timer by 1;
	if the reservoir-empty-timer is 0:
		now the low-tide is true;
		now the trunk of jewels is zil-visible;
		if the player is in Dam-Room:
			say "The water level behind the dam is now quite low.";
		if the player is in Deep Canyon:
			say "The roar of rushing water is quieter now.";
		if the player is in Reservoir-South or the player is in Reservoir-North:
			say "The water level is now quite low here and you could easily cross over to the other side.";
		if the player is in the magic boat and the location of the player is Reservoir:
			say "The water level has dropped to the point at which the boat can no longer stay afloat. It sinks into the mud."
Every turn when the reservoir-fill-timer > 0 (this is the reservoir filling rule):
	decrease the reservoir-fill-timer by 1;
	if the reservoir-fill-timer is 0:
		now the low-tide is false;
		now the trunk of jewels is zil-invisible;
		if the location of the player is Reservoir:
			if the player is in the magic boat:
				say "The boat lifts gently out of the mud and is now floating on the reservoir.";
			otherwise:
				die saying "You are lifted up by the rising river! You try to swim, but the currents are too strong. You come closer, closer to the awesome structure of Flood Control Dam #3. The dam beckons to you. The roar of the water nearly deafens you, but you remain conscious as you tumble over the dam toward your certain doom among the rocks at its base.";
		if the player is in Dam-Room:
			say "The water level behind the dam is now quite high.";
		if the player is in Deep Canyon:
			say "A sound, like that of flowing water, starts to come from below.";
		if the player is in Reservoir-South or the player is in Reservoir-North:
			say "You notice that the water level has risen to the point that it is impossible to cross."
Every turn when the location of the player is Reservoir and the player is not in the magic boat and the reservoir-fill-timer > 0 and the low-tide is true (this is the reservoir perilous warning rule):
	say "You notice that the water level here is rising rapidly. The currents are also becoming stronger. Staying here seems quite perilous!"
Chapter 15 - Room Entering Points
Chapter 16 - Test Commands
Test cellar with "n / n / u / take egg / d / s / e / open window / w / take sack / take bottle / w / take sword / take lantern / open case / put egg in case / e / turn on lantern / u / take rope / d / open sack / take garlic / w / move rug / open trap door / d".
Test troll with "s / drop sack / drop bottle / drop rope / e / take painting / w / n / n / attack troll / attack troll / attack troll / attack troll / attack troll / attack troll / attack troll / attack troll / attack troll / attack troll / attack troll / attack troll" holding the sword and the garlic.
Test cyclops with "e / e / e / echo / take platinum bar / w / n / ne / e / n / take matchbook / n / press yellow button / take wrench / take screwdriver / s / s / turn bolt / drop wrench / s / sw / s / w / w / w / s / e / u / sw / e / s / se / odysseus" holding the sword and the brass lantern.
Test dam with "n / ne / e / n / take matchbook / n / press yellow button / take wrench / take screwdriver / s / s / turn bolt / drop wrench" holding the brass lantern.
Test exorcism with "se / e / tie rope to railing / d / take torch / s / take bell / s / take book / take candles / d / d / ring bell / take candles / light match / light candles with match / read book / drop book / drop candles / s / take skull" holding the brass lantern and the rope and the matchbook.
Test machine with "n / d / take bracelet / e / ne / se / sw / d / d / s / take coal / n / u / u / n / e / s / n / u / s / put coal in basket / lower basket / n / d / e / ne / se / sw / d / d / w / drop lantern / w / take torch / take coal / take screwdriver / s / open lid / put coal in machine / close lid / turn on switch / drop screwdriver / open lid / take diamond" holding the brass lantern and the screwdriver.
Test boat with "ne / e / turn off lantern / d / inflate plastic / drop pump / turn on lantern / enter boat / launch" holding the brass lantern and the pile of plastic and the air pump.
-}