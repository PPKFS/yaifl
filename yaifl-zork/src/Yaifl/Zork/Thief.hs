module Yaifl.Zork.Thief where

{-
TODO
Part 4 - The Thief
Chapter 1 - Thief NPC
-}

{-
The thief is a person. "There is a suspicious-looking individual, holding a bag, leaning against one wall. He is armed with a vicious-looking stiletto."
Understand "thief" and "robber" and "man" and "person" and "shady" and "suspicious" and "seedy" as the thief.
The thief is in Round Room.
-}

{-
The thief-strength is a number that varies. The thief-strength is 5.
The thief-unconscious is a truth state that varies. The thief-unconscious is false.
-}

{-
The large bag is carried by the thief. The large bag is a container. The carrying capacity of the large bag is 100.
Understand "bag" and "large" and "thief's" as the large bag.
-}

{-
The description of the large bag is "[if the thief is defeated]The bag is closed and you can[apostrophe]t see what[apostrophe]s inside.[otherwise]The bag is underneath the thief, so one can[apostrophe]t say what, if anything, is inside.[end if]"
-}

{-
The stiletto is carried by the thief. The stiletto is a weapon.
Understand "stiletto" and "vicious" as the stiletto.
The description of the stiletto is "It's a vicious-looking stiletto."
-}

{-
Instead of taking the stiletto when the thief is not defeated and the thief carries the stiletto:
  say "The thief swings it out of your reach."
-}

{-
Instead of taking the large bag:
  if the thief is defeated:
    continue the action;
  otherwise if the thief-unconscious is true:
    say "Sadly for you, the robber collapsed on top of the bag. Trying to take it would wake him.";
-}

{-
  otherwise:
    say "The bag will be taken over his dead body."
-}

{-
Instead of inserting something into the large bag:
  say "It would be a good trick."
Instead of opening the large bag:
  say "Getting close enough would be a good trick."
Instead of closing the large bag:
  say "Getting close enough would be a good trick."
-}

{-
The description of the thief is "The thief is a slippery character with beady eyes that flit back and forth. He carries, along with an unmistakable arrogance, a large bag over his shoulder and a vicious stiletto, whose blade is aimed menacingly in your direction. I[apostrophe]d watch out if I were you."
-}

{-
Instead of listening to the thief:
  say "The thief says nothing, as you have not been formally introduced."
Instead of taking the thief:
  say "Once you got him, what would you do with him?"
-}

{-
The thief-active is a truth state that varies. The thief-active is true.
The thief-here-count is a number that varies. The thief-here-count is 0.
The thief-engrossed is a truth state that varies. The thief-engrossed is false.
The thief-timer is a number that varies. The thief-timer is 0.
-}

{-
Every turn when the thief is not defeated and the thief-active is true (this is the thief daemon rule):
  increase the thief-timer by 1;
  if the thief-timer < 5:
    do nothing;
-}

{-
  otherwise:
    now the thief-timer is 0;
    let thief-room be the location of the thief;
    let player-room be the location of the player;
-}

{-
    if thief-room is player-room:
      [Thief encounters the player]
      if the player carries the clove of garlic:
        do nothing;
-}

{-
      otherwise if a random chance of 3 in 10 succeeds:
        [Rob the player of all valuables at 75% each]
        let stolen-any be false;
        let lost-light be false;
-}

{-
        repeat with item running through things carried by the player:
          if the treasure-value of item > 0:
            if a random chance of 3 in 4 succeeds:
              if item is lit:
                now lost-light is true;
              now item is in the large bag;
              now stolen-any is true;
-}

{-
        if stolen-any is true:
          play the sound of laugh-sfx as sfx;
          say "The thief just left, still carrying his large bag. You may not have noticed that he robbed you blind first.";
          if lost-light is true:
            say "[line break]The thief seems to have left you in the dark.";
-}

{-
        otherwise:
          say "The thief, finding nothing of value, left disgusted.";
        [Thief flees after encounter]
        let new-dest be a random dark room that is in the Underground;
        if new-dest is a room:
          move the thief to new-dest;
-}

{-
    otherwise:
      [Thief is in a room without the player - steal and move]
      repeat with item running through things in thief-room:
        if the treasure-value of item > 0:
          if a random chance of 3 in 4 succeeds:
            now item is in the large bag;
-}

{-
      [Move to a random underground room]
      let new-dest be a random dark room that is in the Underground;
      if new-dest is a room and new-dest is not player-room:
        move the thief to new-dest.
-}

{-
Every turn when the jewel-encrusted egg is in the large bag and the jewel-encrusted egg is closed (this is the thief opens egg rule):
  now the jewel-encrusted egg is open.
-}

{-
Before looking when the player is in Treasure Room and the thief is not defeated and the thief is not in Treasure Room (this is the thief lair rule):
  move the thief to Treasure Room;
  say "You hear a scream of anguish as you violate the robber[apostrophe]s hideaway. Using passages unknown to you, he rushes to its defense.";
-}

{-
  let found-treasure be false;
  repeat with item running through things in the Treasure Room:
    if item is not the thief and item is not the chalice and item is not the player:
      if the treasure-value of item > 0:
        now found-treasure is true;
        now item is zil-invisible;
-}

{-
  if found-treasure is true:
    say "[line break]The thief gestures mysteriously, and the treasures in the room suddenly vanish.[line break]".
-}

{-
Instead of answering the thief that "hello":
  if the thief is defeated:
    say "Unfortunately, the thief can[apostrophe]t hear you.";
  otherwise if the thief-unconscious is true:
    say "The thief, being temporarily incapacitated, is unable to acknowledge your greeting with his usual graciousness.";
  otherwise:
    say "The thief says nothing, as you have not been formally introduced."
-}

{-
Instead of telling the thief about something:
  if the thief is defeated:
    say "Unfortunately, the thief can[apostrophe]t hear you.";
  otherwise if the thief-unconscious is true:
    say "The thief, being temporarily incapacitated, is unable to respond.";
  otherwise:
    say "The thief is a strong, silent type."
-}

{-
Instead of giving something to the thief:
  if the thief is defeated:
    say "He's not exactly in a position to accept gifts.";
-}

{-
  otherwise:
    now the noun is in the large bag;
    if the treasure-value of the noun > 0:
      now the thief-engrossed is true;
      say "The thief is taken aback by your unexpected generosity, but accepts the [noun] and stops to admire its beauty.";
-}

{-
    otherwise:
      say "The thief places the [noun] in his bag and thanks you politely."
-}

{-
Instead of attacking the thief:
  if the thief is not in the location of the player:
    say "There is no thief here." instead;
  if the thief is defeated:
    say "The thief is already dead.";
-}

{-
  otherwise:
    let W be a random weapon carried by the player;
    if W is nothing:
      say "Trying to attack the thief with your bare hands is suicidal.";
-}

{-
    otherwise:
      now the melee-weapon is W;
      now the melee-target is the thief;
      let hit-chance be a random number between 1 and 10;
-}

{-
      if hit-chance is at least 3:
        decrease the thief-strength by 1;
        if the thief-strength is at most 0:
          now the thief is defeated;
          now the stiletto is in the location of the player;
-}

{-
          repeat with item running through things in the large bag:
            now item is in the location of the player;
-}

{-
          if the location of the player is Treasure Room:
            repeat with item running through zil-invisible things in the Treasure Room:
              now item is zil-visible;
-}

{-
            print hero melee for "kill";
            say "[line break][sinister-black-fog for the thief][paragraph break]As the thief dies, the power of his magic decreases, and his treasures reappear:";
-}

{-
            repeat with item running through things in the location of the player:
              if item is not the chalice and item is not the thief and item is not the player and item is not the large bag:
                say "[line break]  A [item]";
-}

{-
                if item is an open container and something is in item:
                  say ", with ";
                  let contents-count be 0;
                  repeat with sub-item running through things in item:
                    increase contents-count by 1;
-}

{-
                  let printed be 0;
                  repeat with sub-item running through things in item:
                    increase printed by 1;
                    if printed > 1 and printed is contents-count:
                      say ", and ";
                    otherwise if printed > 1:
                      say ", ";
                    say "a [sub-item]";
-}

{-
            say "[paragraph break]The chalice is now safe to take.";
          otherwise:
            print hero melee for "kill";
            say "[line break][sinister-black-fog for the thief]";
          remove the thief from play;
-}

{-
        otherwise:
          print hero melee for "light-wound";
      otherwise:
        print hero melee for "miss".
-}
