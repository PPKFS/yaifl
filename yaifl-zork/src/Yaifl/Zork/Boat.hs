module Yaifl.Zork.Boat where


{- TODO: Chapter 11 - Boat System -}

{-
Understand "boat" and "raft" and "magic" and "plastic" and "seaworthy" and "inflat" as the magic boat.
The description of the magic boat is "It's a seaworthy magic boat."
-}

{-
The punctured boat is a thing. "There is a large punctured boat here."
Understand "boat" and "pile" and "plastic" and "punctured" and "large" as the punctured boat.
The description of the punctured boat is "It's a punctured boat beyond repair."
-}

{-
Instead of inflating the punctured boat: say "No chance. Some moron punctured it."
-}

{-
Instead of plugging the punctured boat with the viscous material:
  say "Well done. The boat is repaired.";
  now the punctured boat is nowhere;
  now the pile of plastic is in the location of the player.
-}

{-
The tan label is a thing. The tan label is in the magic boat.
Understand "label" and "fineprint" and "print" and "tan" and "fine" as the tan label.
-}

{-
The description of the tan label is "!!!!FROBOZZ MAGIC BOAT COMPANY!!!![paragraph break]Hello, Sailor![paragraph break]Instructions for use:[paragraph break]   To get into a body of water, say 'Launch'.[line break]   To get to shore, say 'Land' or the direction in which you want to maneuver the boat.[paragraph break]Warranty:[line break]  This boat is guaranteed against all defects for a period of 76 milliseconds from date of purchase or until first used, whichever comes first.[paragraph break]Warning:[line break]   This boat is made of thin plastic.[line break]   Good Luck!"
-}

{-
The boat-inflated is a truth state that varies. The boat-inflated is false.
The boat-punctured is a truth state that varies. The boat-punctured is false.
-}

{-
Inflating is an action applying to one thing. Understand "inflate [something]" and "pump up [something]" as inflating.
Carry out inflating: say "You can't inflate that."
-}

{-
Inflating-with is an action applying to two things. Understand "inflate [something] with [something]" and "pump up [something] with [something]" and "fill [something] with [something]" as inflating-with.
-}

{-
Instead of inflating-with the pile of plastic:
  if the second noun is the hand-held air pump:
    try inflating the pile of plastic;
  otherwise:
    say "With a [second noun]? Surely you jest!"
-}

{-
Instead of inflating-with:
  say "You can[apostrophe]t inflate that."
-}

{-
Instead of inflating the pile of plastic:
  if the pile of plastic is not in the location of the player:
    say "The boat must be on the ground to be inflated.";
  otherwise if the player carries the hand-held air pump or the hand-held air pump is in the location of the player:
    play the sound of inflate-sfx as sfx;
    say "The boat inflates and appears seaworthy.";
    if the tan label is not handled:
      say "A tan label is lying inside the boat.";
    now the boat-inflated is true;
    now the pile of plastic is nowhere;
    now the magic boat is in the location of the player;
  otherwise:
    say "You don[apostrophe]t have enough lung power to inflate it."
-}

{-
Instead of inflating the magic boat:
  say "Inflating it further would probably burst it."
-}

{-
Instead of reading the magic boat:
  say "Read the label for the boat's instructions."
-}

{-
Instead of deflating the magic boat when the player is in the magic boat and the location of the player is River1 or the location of the player is River2 or the location of the player is River3 or the location of the player is River4 or the location of the player is River5:
  die saying "You realize that getting out here would be fatal."
-}

{-
Deflating is an action applying to one thing. Understand "deflate [something]" as deflating.
Carry out deflating: say "You can't deflate that."
-}

{-
Instead of deflating the magic boat:
  if the player is in the magic boat:
    say "You can[apostrophe]t deflate the boat while you[apostrophe]re in it." instead;
  say "The boat deflates.";
  now the boat-inflated is false;
  let here be the location of the magic boat;
  repeat with item running through things in the magic boat:
    now item is in here;
  now the magic boat is nowhere;
  now the pile of plastic is in here.
-}

{-
The nonland-room is a truth state that varies. The nonland-room is false.
-}

{-
To decide whether on-water:
  let here be the location of the player;
  if here is River1 or here is River2 or here is River3 or here is River4 or here is River5:
    decide yes;
  if here is Reservoir or here is In-Stream:
    decide yes;
  decide no.
-}

{-
Instead of dropping a weapon when the player is in the magic boat and on-water:
  say "It seems that the [noun] doesn[apostrophe]t agree with the boat, as evidenced by the loud hissing noise issuing therefrom. With a pathetic sputter, the boat deflates, leaving you without.";
  let here be the location of the player;
  now the magic boat is nowhere;
  now the punctured boat is in here;
-}

{-
  if here is Reservoir or here is In-Stream:
    die saying "[line break]Another pathetic sputter, this time from you, heralds your drowning.";
  otherwise:
    die saying "[line break]In other words, fighting the fierce currents of the Frigid River. You manage to hold your own for a bit, but then you are carried over a waterfall and into some nasty rocks. Ouch!"
-}

{-
Report entering the magic boat:
  say "You are now in the magic boat." instead.
-}

{-
Before entering the magic boat:
  let sharp-items be false;
  if the player carries the sword:
    now sharp-items is true;
  if the player carries the sceptre:
    now sharp-items is true;
  if the player carries the nasty knife:
    now sharp-items is true;
  if the player carries the rusty knife:
    now sharp-items is true;
-}

{-
  if sharp-items is true:
    say "Oops! Something sharp seems to have slipped and punctured the boat. The boat deflates to the sounds of hissing, sputtering, and cursing.";
    now the boat-punctured is true;
    let here be the location of the player;
    repeat with item running through things in the magic boat:
      now item is in here;
    now the magic boat is nowhere;
    now the punctured boat is in here;
    stop the action.
-}

{-
Launching is an action applying to nothing. Understand "launch" as launching.
-}

{-
Carry out launching:
  let here be the location of the player;
  if the player is not in the magic boat:
    say "You[apostrophe]re not in the boat!";
-}

{-
  otherwise if here is River1 or here is River2 or here is River3 or here is River4 or here is River5:
    say "You are on the river, or have you forgotten?";
  otherwise if here is Reservoir:
    say "You are on the reservoir, or have you forgotten?";
  otherwise if here is In-Stream:
    say "You are on the stream, or have you forgotten?";
-}

{-
  otherwise if here is Dam-Base:
    say "You push off from the shore.";
    move the magic boat to River1;
    now the river-current-active is true;
    now the river-current-timer is 2;
    try looking;
-}

{-
  otherwise if here is White Cliffs North:
    say "You push off from the shore.";
    move the magic boat to River3;
    now the river-current-active is true;
    now the river-current-timer is 1;
    try looking;
-}

{-
  otherwise if here is White Cliffs South:
    say "You push off from the shore.";
    move the magic boat to River4;
    now the river-current-active is true;
    now the river-current-timer is 2;
    try looking;
-}

{-
  otherwise if here is Shore:
    say "You push off from the shore.";
    move the magic boat to River5;
    now the river-current-active is true;
    now the river-current-timer is 1;
    try looking;
-}

{-
  otherwise if here is Sandy Beach:
    say "You push off from the shore.";
    move the magic boat to River4;
    now the river-current-active is true;
    now the river-current-timer is 2;
    try looking;
-}

{-
  otherwise if here is Reservoir-South or here is Reservoir-North:
    say "You push off from the shore.";
    move the magic boat to Reservoir;
    try looking;
-}

{-
  otherwise if here is Stream View:
    say "You push off from the shore.";
    move the magic boat to In-Stream;
    try looking;
-}

{-
  otherwise:
    say "You're not near any water."
-}
