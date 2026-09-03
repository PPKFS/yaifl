module Yaifl.Zork.Cyclops where
{-
TODO
Chapter 9 - Cyclops NPC
-}

{-
The cyclops is a person in Cyclops-Room.
Understand "cyclops" and "monster" and "eye" and "hungry" and "giant" as the cyclops.
Rule for writing a paragraph about the cyclops: now the cyclops is mentioned.
The description of the cyclops is "A hungry cyclops is blocking the staircase, looking at you as if you were a potential meal."
-}

{-
The cyclops-fed is a truth state that varies. The cyclops-fed is false.
The cyclops-watered is a truth state that varies. The cyclops-watered is false.
The cyclops-asleep is a truth state that varies. The cyclops-asleep is false.
-}

{-
The cyclops-wrath is a number that varies. The cyclops-wrath is 0.
The cyclops-wrath-timer is a number that varies. The cyclops-wrath-timer is 0.
-}

{-
Instead of giving the lunch to the cyclops:
  if the cyclops-asleep is true:
    say "No use. He's fast asleep.";
  otherwise:
    remove the lunch from play;
    now the cyclops-fed is true;
    decrease the cyclops-wrath by 1;
    now the cyclops-wrath-timer is 1;
    say "The cyclops says [quotation mark]Mmm Mmm. I love hot peppers! But oh, could I use a drink. Perhaps I could drink the blood of that thing.[quotation mark] From the gleam in his eye, it could be surmised that you are [quotation mark]that thing.[quotation mark]"
-}

{-
Instead of giving the quantity of water to the cyclops:
  if the cyclops-asleep is true:
    say "No use. He's fast asleep.";
  otherwise if the cyclops-fed is false:
    say "The cyclops apparently is not thirsty and refuses your generous offer.";
  otherwise:
    remove the quantity of water from play;
    now the cyclops-watered is true;
    now the cyclops-asleep is true;
    now the cyclops-flag is true;
    say "The cyclops takes the bottle, checks that it's open, and drinks the water. A moment later, he lets out a yawn that nearly blows you over, and then falls fast asleep (what did you put in that drink, anyway?)."
-}

{-
Instead of giving the glass bottle to the cyclops when the quantity of water is in the glass bottle:
  if the cyclops-asleep is true:
    say "No use. He's fast asleep.";
  otherwise if the cyclops-fed is false:
    say "The cyclops apparently is not thirsty and refuses your generous offer.";
  otherwise:
    remove the quantity of water from play;
    now the cyclops-watered is true;
    now the cyclops-asleep is true;
    now the cyclops-flag is true;
    say "The cyclops takes the bottle, checks that it's open, and drinks the water. A moment later, he lets out a yawn that nearly blows you over, and then falls fast asleep (what did you put in that drink, anyway?)."
-}

{-
Instead of giving something to the cyclops:
  if the cyclops-asleep is true:
    say "No use. He's fast asleep.";
  otherwise if the noun is the clove of garlic:
    say "The cyclops may be hungry, but there is a limit.";
  otherwise if the noun is not the lunch and the noun is not the quantity of water and the noun is not the glass bottle:
    say "The cyclops is not so stupid as to eat THAT!"
-}

{-
Instead of attacking the cyclops:
  if the cyclops-asleep is true:
    say "The cyclops yawns and stares at the thing that woke him up.";
    now the cyclops-asleep is false;
    now the cyclops-flag is false;
  otherwise:
    increase the cyclops-wrath by 1;
    now the cyclops-wrath-timer is 1;
    say "The cyclops shrugs but otherwise ignores your pitiful attempt."
-}

{-
Instead of telling the cyclops about something:
  if the cyclops-asleep is true:
    say "No use talking to him. He[apostrophe]s fast asleep.";
  otherwise:
    say "The cyclops prefers eating to making conversation."
-}

{-
Instead of examining the cyclops when the cyclops-asleep is true:
  say "The cyclops is sleeping like a baby, albeit a very ugly one."
-}

{-
Instead of pushing the cyclops:
  say "[quotation mark]Do you think I[apostrophe]m as stupid as my father was?[quotation mark], he says, dodging."
-}

{-
Instead of taking the cyclops:
  say "The cyclops doesn[apostrophe]t take kindly to being grabbed."
-}

{-
Instead of tying the cyclops to something:
  say "You cannot tie the cyclops, though he is fit to be tied."
-}

{-
Instead of listening to the cyclops:
  say "You can hear his stomach rumbling."
-}

{-
Every turn when the cyclops-wrath-timer > 0 and the player is in Cyclops-Room and the cyclops-asleep is false (this is the cyclops wrath rule):
  increase the cyclops-wrath-timer by 1;
  if the cyclops-wrath > 5 or the cyclops-wrath < -5:
    die saying "The cyclops, tired of all of your games and trickery, grabs you firmly. As he licks his chops, he says [quotation mark]Mmm. Just like Mom used to make [apostrophe]em.[quotation mark] It[apostrophe]s nice to be appreciated.";
-}

{-
  otherwise if the cyclops-wrath > 0:
    if the cyclops-wrath is 1:
      say "The cyclops seems somewhat agitated.";
    otherwise if the cyclops-wrath is 2:
      say "The cyclops appears to be getting more agitated.";
    otherwise if the cyclops-wrath is 3:
      say "The cyclops is moving about the room, looking for something.";
    otherwise if the cyclops-wrath is 4:
      say "The cyclops was looking for salt and pepper. No doubt they are condiments for his upcoming snack.";
    otherwise:
      say "You have two choices: 1. Leave  2. Become dinner.";
-}

{-
  otherwise if the cyclops-wrath < 0:
    say "The cyclops, having eaten the hot peppers, appears to be gasping. His enflamed tongue protrudes from his man-sized mouth."
-}

{-
Odysseusing is an action applying to nothing.
Understand "odysseus" and "ulysses" as odysseusing.
-}

{-
Carry out odysseusing:
  if the player is in Cyclops-Room and the cyclops is in Cyclops-Room and the cyclops-asleep is false:
    play the sound of footsteps-sfx as sfx;
    say "The cyclops, hearing the name of his father's deadly nemesis, flees the room by knocking down the wall on the east of the room.";
    remove the cyclops from play;
    now the cyclops-flag is true;
    now the magic-flag is true;
-}

{-
  otherwise if the player is in Cyclops-Room and the cyclops-asleep is true:
    say "The cyclops is asleep and can't hear you.";
  otherwise:
    say "Wasn't he a sailor?"
-}

{-
The chalice is in Treasure Room. "There is a silver chalice, intricately engraved, here."
Understand "chalice" and "cup" and "silver" as the chalice.
The description of the chalice is "It's a beautifully engraved silver chalice."
The treasure-value of the chalice is 5.
The point-value of the chalice is 10.
-}

{-
Instead of inserting something into the chalice:
  say "You can[apostrophe]t. It[apostrophe]s not a very good chalice, is it?"
-}

{-
Instead of taking the chalice when the player is in Treasure Room and the thief is not defeated and the thief is in Treasure Room:
  say "You[apostrophe]d be stabbed in the back first."
-}
