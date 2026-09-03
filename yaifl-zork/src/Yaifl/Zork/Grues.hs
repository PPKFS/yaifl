module Yaifl.Zork.Grues where

{-
TODO
Chapter 5 - Darkness and Grues
-}

{-
[ZIL grue behavior: attacks only on movement, never on non-movement actions.
Two triggers: (1) V-WALK — trying to go a direction with no exit while in dark,
(2) GOTO — successfully moving from one dark room to another dark room.
Moving from a lit room into a dark room is always safe (warning only).]
-}

{-
The was-in-dark is a truth state that varies. The was-in-dark is false.
-}

{-
Rule for printing the description of a dark room:
  if the always-lit-mode is false:
    say "It is pitch black. You are likely to be eaten by a grue.[line break]" instead;
  otherwise:
    say "It is pitch black.[line break]" instead.
-}

{-
Rule for implicitly taking something (called the thing taken):
  say "(Taken)[command clarification break]";
  silently try taking the thing taken.
-}

{-
To grue-death:
  play the sound of grue-sfx as sfx;
  let R be a random number between 1 and 3;
-}

{-
  if R is 1:
    die saying "Oh, no! A lurking grue slithered into the room and devoured you!";
  otherwise if R is 2:
    die saying "Oh, no! You have walked into the slavering fangs of a lurking grue!";
  otherwise:
    die saying "Oh, no! You have walked into a den of hungry grues and it[apostrophe]s dinner time!".
-}

{-
[Record lighting state before any movement attempt]
Before going (this is the save darkness state rule):
  if in darkness:
    now the was-in-dark is true;
  otherwise:
    now the was-in-dark is false;
  continue the action.
-}

{-
[ZIL GOTO check: moving dark-to-dark has 80% grue death]
After going when the was-in-dark is true and in darkness (this is the dark-to-dark grue rule):
  if the always-lit-mode is false and a random chance of 4 in 5 succeeds:
    grue-death;
  continue the action.
-}

{-
[ZIL V-WALK check: trying to move in a direction with no exit while in dark]
Instead of going nowhere when in darkness (this is the dark-movement grue rule):
  if the always-lit-mode is false and a random chance of 4 in 5 succeeds:
    grue-death;
  say "You can[apostrophe]t go that way."
-}

{-
After deciding the scope of the player when in darkness:
  repeat with item running through things enclosed by the location:
    place item in scope.
-}
