module Yaifl.Zork.World.Barrow where

{-
TODO
Chapter 20 - Stone Barrow and Endgame
Stone Barrow is a room. "You are standing in front of a massive barrow of stone. In the east face is a huge stone door which is open. You cannot see into the dark of the tomb."
Northeast of Stone Barrow is West-of-House.
Instead of going southwest in West-of-House:
  if the won-flag is true:
    move the player to Stone Barrow;
  otherwise:
    say "You can't go that way."
The barrow-door is scenery in Stone Barrow. The printed name of the barrow-door is "stone door".
Understand "door" and "stone" and "huge" as the barrow-door.
The description of the barrow-door is "The door is a huge slab of stone."
The barrow-facade is scenery in Stone Barrow. The printed name of the barrow-facade is "barrow".
Understand "barrow" and "tomb" and "massive" as the barrow-facade.
The description of the barrow-facade is "It's a massive barrow of stone."
Instead of entering the barrow-facade: try going inside.
Instead of entering the barrow-door: try going inside.
Instead of opening the barrow-door: say "The door is too heavy."
Instead of closing the barrow-door: say "The door is too heavy."
Instead of going inside in Stone Barrow:
  say "Inside the Barrow[line break]As you enter the barrow, the door closes inexorably behind you. Around you it is dark, but ahead is an enormous cavern, brightly lit. Through its center runs a wide stream. Spanning the stream is a small wooden footbridge, and beyond a path leads into a dark tunnel. Above the bridge, floating in the air, is a large sign. It reads: All ye who stand before this bridge have completed a great and perilous adventure which has tested your wit and courage. You have mastered the first part of the ZORK trilogy. Those who pass over this bridge must be prepared to undertake an even greater adventure that will severely test your skill and bravery![paragraph break]The ZORK trilogy continues with 'ZORK II: The Wizard of Frobozz' and is completed in 'ZORK III: The Dungeon Master.'[line break]";
  end the story finally saying "Congratulations!"
Instead of going west in Stone Barrow:
  try going inside.
Chapter 21 - In-Stream
In-Stream is a room. The printed name of In-Stream is "Stream". "You are on the gently flowing stream. The upstream route is too narrow to navigate, and the downstream route is invisible due to twisting walls. There is a narrow beach to land on."
In-Stream is in the Underground.
Up from Reservoir is In-Stream. West of Reservoir is In-Stream.
Down from In-Stream is Reservoir. East of In-Stream is Reservoir.
Instead of going up in In-Stream:
  say "The channel is too narrow."
Instead of going west in In-Stream:
  say "The channel is too narrow."
Landing is an action applying to nothing. Understand "land" as landing.
Carry out landing:
  if the player is in In-Stream:
    move the player to Stream View;
  otherwise:
    say "You're not on the water."

-}