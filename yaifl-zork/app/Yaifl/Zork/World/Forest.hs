module Yaifl.Zork.World.Forest where

{-
TODO
Section 4 - Forest Rooms
Forest1 is a room. The printed name of Forest1 is "Forest". "This is a forest, with trees in all directions. To the east, there appears to be sunlight. A faint breeze stirs the branches overhead, carrying the earthy scent of decaying leaves and damp moss."
Forest1 is in Forest Area.
Forest1 is west of West-of-House.
Forest2 is a room. The printed name of Forest2 is "Forest". "This is a dimly lit forest, with large trees all around. The canopy here is thick, allowing only thin shafts of light to reach the forest floor. A carpet of pine needles muffles your footsteps."
Forest2 is in Forest Area.
Mountains is a room. The printed name of Mountains is "Forest". "The forest thins out, revealing impassable mountains."
Forest3 is a room. The printed name of Forest3 is "Forest". "This is a dimly lit forest, with large trees all around. Gnarled roots break through the soil underfoot, and the air is heavy with the smell of wet bark. Somewhere nearby, water drips steadily from the leaves."
Forest3 is in Forest Area.
Forest3 is south of South-of-House.
Northwest of Forest3 is South-of-House.
West of Forest3 is Forest1. South of Forest1 is Forest3.
Forest Path is a room. "This is a path winding through a dimly lit forest. The path heads north-south here. One particularly large tree with some low branches stands at the edge of the path."
Forest Path is in Forest Area.
North of North-of-House is Forest Path.
South of Forest Path is North-of-House. East of Forest Path is Forest2. West of Forest Path is Forest1. North of Forest1 is Grating Clearing.
East of Forest1 is Forest Path.
Forest2 is east of Mountains. Forest2 is north of Mountains. Forest2 is south of Mountains. Forest2 is west of Mountains.
North of Forest2 is nowhere. South of Forest2 is Clearing. West of Forest2 is Forest Path. East of Forest2 is Mountains.
North of Forest3 is Clearing. East of Forest3 is nowhere.
Instead of going north in Forest2:
  say "The forest becomes impenetrable to the north."
Instead of going east in Forest3:
  say "The rank undergrowth prevents eastward movement."
Instead of going south in Forest3:
  say "Storm-tossed trees block your way."
Instead of going up in Forest1:
  say "There is no tree here suitable for climbing."
Instead of going up in Forest2:
  say "There is no tree here suitable for climbing."
Instead of going up in Forest3:
  say "There is no tree here suitable for climbing."
The mountain-range is scenery in Mountains. The printed name of the mountain-range is "mountains".
Understand "mountain" and "mountains" and "range" and "impassable" and "flathead" as the mountain-range.
The description of the mountain-range is "The mountains are impassable."
Instead of climbing the mountain-range: say "Don[apostrophe]t you believe me? The mountains are impassable!"
Instead of going up in Mountains:
  say "The mountains are impassable."
Instead of going east in Mountains:
  say "The mountains are impassable."
Instead of going west in Forest1:
  say "You would need a machete to go further west."
Up a Tree is a room. "You are about 10 feet above the ground nestled among some large branches. The nearest branch above you is above your reach."
Up a Tree is in Forest Area.
Up a Tree is above Forest Path.
Instead of going up in Up a Tree:
  say "You cannot climb any higher."
After looking in Up a Tree:
  let item-list be a list of things;
  repeat with item running through things in Forest Path:
    if the item is not scenery and the item is not undescribed:
      add item to item-list;
  if the number of entries in item-list > 0:
    say "On the ground below you can see: [item-list with indefinite articles]."
Clearing is a room. "You are in a small clearing in a well marked forest path that extends to the east and west."
Clearing is in Forest Area.
North of Clearing is Forest2. South of Clearing is Forest3. West of Clearing is Behind House.
[East of Clearing is connected in Phase 7 - see Canyon View]
Instead of going up in Clearing:
  say "There is no tree here suitable for climbing."
Grating Clearing is a room. The printed name of Grating Clearing is "Clearing".
The description of Grating Clearing is "You are in a clearing, with a forest surrounding you on all sides. A path leads south.[if the grate is open][line break]There is an open grating, descending into darkness.[otherwise if the grate-revealed is true][line break]There is a grating securely fastened into the ground.[end if]".
Grating Clearing is in Forest Area.
North of Forest Path is Grating Clearing. East of Grating Clearing is Forest2. West of Grating Clearing is Forest1. South of Grating Clearing is Forest Path.
Instead of going north in Grating Clearing:
  say "The forest becomes impenetrable to the north."
Instead of going down in Grating Clearing:
  if the grate is not visible:
    say "You can't go that way." instead;
  if the grate is open:
    say "(through the grating)[line break]";
    move the player to Grating Room instead;
  otherwise:
    say "The grating is closed!" instead.
Section 5 - Songbird Ambient
The forest-songbird is a backdrop. The printed name of the forest-songbird is "songbird".
Understand "bird" and "songbird" and "song" as the forest-songbird.
The forest-songbird is in Forest Area.
The description of the forest-songbird is "The songbird is not here but is probably nearby."
Instead of taking the forest-songbird:
  say "The songbird is not here but is probably nearby."
Instead of listening to the forest-songbird:
  say "You can't hear the songbird now."
Every turn when the player is in the Forest Area (this is the songbird singing rule):
  if a random chance of 15 in 100 succeeds:
    play the sound of bird-sfx as sfx;
    say "You hear in the distance the chirping of a song bird.[line break]".
Section 5a - Forest Pseudo-Object
The forest-pseudo is a backdrop. The forest-pseudo is in Forest Area.
The printed name of the forest-pseudo is "forest".
Understand "forest" as the forest-pseudo when the player is in Forest Area.
The description of the forest-pseudo is "You cannot see the forest for the trees."
Instead of finding the forest-pseudo: say "You cannot see the forest for the trees."
Instead of listening to the forest-pseudo: say "The pines and the hemlocks seem to be murmuring."
Instead of exiting when the player is in Forest Area and the player is not in the magic boat: say "You will have to specify a direction."
Instead of following the forest-songbird: say "It can't be followed."
Section 6 - Forest Trees
The forest-trees is a backdrop. The printed name of the forest-trees is "trees".
Understand "tree" and "trees" and "branch" and "large" and "forest" and "pines" and "hemlocks" as the forest-trees.
The forest-trees is in Forest Area.
The description of the forest-trees is "The trees are tall and closely grown."
Instead of listening to the forest-trees:
  say "The pines and the hemlocks seem to be murmuring."
Instead of climbing the forest-trees when the player is in Forest Path:
  try going up.
Instead of climbing the forest-trees when the player is in Up a Tree:
  try going up.
-}