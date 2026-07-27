module Yaifl.Zork.World.Canyon where

{-
Chapter 17 - River and Falls Area
River1 is a room. The printed name of River1 is "Frigid River". "You are on the Frigid River in the vicinity of the Dam. The river flows quietly here. There is a landing on the west shore."
River1 is in the Underground.
West of River1 is Dam-Base. Down from River1 is River2.
The river-pseudo is a backdrop. The river-pseudo is in River1, River2, River3, River4, River5, Dam-Base, Shore, Sandy Beach, and Aragain Falls.
The printed name of the river-pseudo is "river".
Understand "river" and "frigid" and "frigid river" as the river-pseudo.
The description of the river-pseudo is "The Frigid River flows swiftly by."
Instead of inserting yourself into the river-pseudo:
  die saying "You splash around for a moment, then you drown."
Instead of inserting something into the river-pseudo:
  if the noun is the pile of leaves or the noun is the sack or the noun is the book or the noun is the bird's nest or the noun is the rope:
    say "The [noun] floats for a moment, then sinks.";
  otherwise:
    say "The [noun] splashes into the water and is gone forever.";
  remove the noun from play.
Instead of jumping when the player can see the river-pseudo and the player is not in the magic boat:
  say "A look before leaping reveals that the river is wide and dangerous, with swift currents and large, half-hidden rocks. You decide to forgo your swim."
The wclif-object is a backdrop. The wclif-object is in White Cliffs North, White Cliffs South, Dam-Base, River1, River2, River3, and River4.
The printed name of the wclif-object is "cliffs".
Understand "cliff" and "cliffs" and "white" as the wclif-object.
The description of the wclif-object is "The White Cliffs tower above you."
Instead of climbing the wclif-object: say "The cliff is too steep for climbing."
Instead of going up in River1:
  say "You cannot go upstream due to strong currents."
Instead of going east in River1:
  say "The White Cliffs prevent your landing here."
River2 is a room. The printed name of River2 is "Frigid River". "The river turns a corner here making it impossible to see the Dam. The White Cliffs loom on the east bank and large rocks prevent landing on the west."
River2 is in the Underground.
Down from River2 is River3.
Instead of going up in River2:
  say "You cannot go upstream due to strong currents."
Instead of going east in River2:
  say "The White Cliffs prevent your landing here."
Instead of going west in River2:
  say "Just in time you steer away from the rocks."
River3 is a room. The printed name of River3 is "Frigid River". "The river descends here into a valley. There is a narrow beach on the west shore below the cliffs. In the distance a faint rumbling can be heard."
River3 is in the Underground.
Down from River3 is River4. West of River3 is White Cliffs North.
Instead of going up in River3:
  say "You cannot go upstream due to strong currents."
White Cliffs North is a room. The printed name of White Cliffs North is "White Cliffs Beach". "You are on a narrow strip of beach which runs along the base of the White Cliffs. There is a narrow path heading south along the Cliffs and a tight passage leading west into the cliffs themselves."
White Cliffs North is in the Underground.
East of Damp Cave is White Cliffs North.
Instead of going south in White Cliffs North:
  if the player carries the pile of plastic or the player does not carry the magic boat:
    move the player to White Cliffs South instead;
  say "The path is too narrow with an inflated boat."
Instead of going west in White Cliffs North:
  if the player carries the pile of plastic or the player does not carry the magic boat:
    move the player to Damp Cave instead;
  say "The path is too narrow with an inflated boat."
White Cliffs South is a room. The printed name of White Cliffs South is "White Cliffs Beach". "You are on a rocky, narrow strip of beach beside the Cliffs. A narrow path leads north along the shore."
White Cliffs South is in the Underground.
Instead of going north in White Cliffs South:
  if the player carries the pile of plastic or the player does not carry the magic boat:
    move the player to White Cliffs North instead;
  say "The path is too narrow with an inflated boat."
River4 is a room. The printed name of River4 is "Frigid River". "The river is running faster here and the sound ahead appears to be that of rushing water. On the east shore is a sandy beach. A small area of beach can also be seen below the cliffs on the west shore."
River4 is in the Underground.
Down from River4 is River5. West of River4 is White Cliffs South. East of River4 is Sandy Beach.
Instead of going up in River4:
  say "You cannot go upstream due to strong currents."
River5 is a room. The printed name of River5 is "Frigid River". "The sound of rushing water is nearly unbearable here. On the east shore is a large landing area."
River5 is in the Underground.
East of River5 is Shore.
Instead of going up in River5:
  say "You cannot go upstream due to strong currents."
Instead of going down in River5:
  die saying "Unfortunately, the magic boat doesn't provide protection from the rocks and boulders one meets at the bottom of waterfalls. Including this one."
Section - River Current System
The river-current-timer is a number that varies. The river-current-timer is 0.
The river-current-active is a truth state that varies. The river-current-active is false.
Every turn when the river-current-active is true (this is the river current rule):
  let here be the location of the player;
  decrease the river-current-timer by 1;
  if the river-current-timer is at most 0:
    if here is River1:
      say "The flow of the river carries you downstream.[line break]";
      move the magic boat to River2;
      now the river-current-timer is 2;
      try looking;
    otherwise if here is River2:
      say "The flow of the river carries you downstream.[line break]";
      move the magic boat to River3;
      now the river-current-timer is 1;
      try looking;
    otherwise if here is River3:
      say "The flow of the river carries you downstream.[line break]";
      move the magic boat to River4;
      now the river-current-timer is 2;
      try looking;
    otherwise if here is River4:
      say "The flow of the river carries you downstream.[line break]";
      move the magic boat to River5;
      now the river-current-timer is 1;
      try looking;
    otherwise if here is River5:
      die saying "Unfortunately, the magic boat doesn't provide protection from the rocks and boulders one meets at the bottom of waterfalls. Including this one.";
    otherwise:
      now the river-current-active is false.
After going to River1:
  now the river-current-active is true;
  now the river-current-timer is 2;
  continue the action.
After going to River2:
  now the river-current-active is true;
  now the river-current-timer is 2;
  continue the action.
After going to River3:
  now the river-current-active is true;
  now the river-current-timer is 1;
  continue the action.
After going to River4:
  now the river-current-active is true;
  now the river-current-timer is 2;
  continue the action.
After going to River5:
  now the river-current-active is true;
  now the river-current-timer is 1;
  continue the action.
After going to Shore:
  if the river-current-active is true:
    say "The magic boat comes to a rest on the shore.[paragraph break]";
  now the river-current-active is false;
  continue the action.
After going to Sandy Beach:
  if the river-current-active is true:
    say "The magic boat comes to a rest on the shore.[paragraph break]";
  now the river-current-active is false;
  continue the action.
After going to White Cliffs North:
  if the river-current-active is true:
    say "The magic boat comes to a rest on the shore.[paragraph break]";
  now the river-current-active is false;
  continue the action.
After going to White Cliffs South:
  if the river-current-active is true:
    say "The magic boat comes to a rest on the shore.[paragraph break]";
  now the river-current-active is false;
  continue the action.
After going to Dam-Base:
  if the river-current-active is true:
    say "The magic boat comes to a rest on the shore.[paragraph break]";
  now the river-current-active is false;
  continue the action.
Shore is a room. "You are on the east shore of the river. The water here seems somewhat treacherous. A path travels from north to south here, the south end quickly turning around a sharp corner."
Shore is in the Underground.
North of Shore is Sandy Beach. South of Shore is Aragain Falls.
Sandy Beach is a room. "You are on a large sandy beach on the east shore of the river, which is flowing quickly by. A path runs beside the river to the south here, and a passage is partially buried in sand to the northeast."
Sandy Beach is in the Underground.
Northeast of Sandy Beach is Sandy Cave. South of Sandy Beach is Shore.
The shovel is in Sandy Beach. "There is a shovel here." Understand "shovel" and "tool" as the shovel.
The description of the shovel is "It's a sturdy shovel."
Sandy Cave is a room. "This is a small, low-ceilinged cave nearly filled with fine white sand. The walls are rough limestone, worn smooth in places by ancient water. The only exit is a narrow passage to the southwest."
Sandy Cave is in the Underground.
Southwest of Sandy Cave is Sandy Beach.
The sand is scenery in Sandy Cave. Understand "sand" as the sand.
The description of the sand is "It's just sand."
The beautiful jeweled scarab is in Sandy Cave. The beautiful jeweled scarab is zil-invisible.
Understand "scarab" and "bug" and "beetle" and "beautiful" and "carved" and "jeweled" as the beautiful jeweled scarab.
The treasure-value of the beautiful jeweled scarab is 5.
The point-value of the beautiful jeweled scarab is 5.
The dig-count is a number that varies. The dig-count is 0.
Digging is an action applying to one thing. Understand "dig [something]" and "dig in [something]" as digging.
Carry out digging:
  say "The ground is too hard for digging here."
Instead of digging the sand:
  if the player does not carry the shovel:
    say "You need a shovel to dig here.";
  otherwise:
    increase the dig-count by 1;
    if the dig-count is 1:
      say "You seem to be digging a hole here.";
    otherwise if the dig-count is 2:
      say "The hole is getting deeper, but that[apostrophe]s about it.";
    otherwise if the dig-count is 3:
      say "You are surrounded by a wall of sand on all sides.";
    otherwise if the dig-count is 4:
      now the beautiful jeweled scarab is zil-visible;
      say "You can see a scarab here in the sand.";
      now the beautiful jeweled scarab is in Sandy Cave;
    otherwise:
      die saying "The hole collapses, smothering you."
Aragain Falls is a room.
The description of Aragain Falls is "You are at the top of Aragain Falls, an enormous waterfall with a drop of about 450 feet. The only path here is on the north end.[if the rainbow-flag is true][line break]A solid rainbow spans the falls.[otherwise][line break]A beautiful rainbow can be seen over the falls and to the west.[end if]".
Aragain Falls is in the Underground.
Instead of going west in Aragain Falls:
  if the rainbow-flag is true:
    move the player to On-the-Rainbow;
  otherwise:
    say "You can't go that way."
Instead of going down in Aragain Falls:
  say "It's a long way..."
On-the-Rainbow is a room. "You are on top of a rainbow (I bet you never thought you would walk on a rainbow), with a magnificent view of the Falls. The rainbow travels east-west here."
The printed name of On-the-Rainbow is "On the Rainbow".
On-the-Rainbow is in the Underground.
West of On-the-Rainbow is End of Rainbow. East of On-the-Rainbow is Aragain Falls.
End of Rainbow is a room. "You are on a small, rocky beach on the continuation of the Frigid River past the Falls. The beach is narrow due to the presence of the White Cliffs. The river canyon opens here and sunlight shines in from above. A rainbow crosses over the falls to the east and a narrow path continues to the southwest."
End of Rainbow is in the Underground.
Southwest of End of Rainbow is Canyon Bottom.
Instead of going east in End of Rainbow:
  if the rainbow-flag is true:
    move the player to On-the-Rainbow;
  otherwise:
    say "You can't go that way."
The pot of gold is in End of Rainbow. The pot of gold is zil-invisible. "At the end of the rainbow is a pot of gold."
Understand "pot" and "gold" as the pot of gold.
The treasure-value of the pot of gold is 10.
The point-value of the pot of gold is 10.
Canyon Bottom is a room. "You are beneath the walls of the river canyon which may be climbable here. The lesser part of the runoff of Aragain Falls flows by below. To the north is a narrow path."
Canyon Bottom is in the Underground.
Up from Canyon Bottom is Rocky Ledge. North of Canyon Bottom is End of Rainbow.
The climbable-cliff is a backdrop. The climbable-cliff is in Canyon Bottom, Rocky Ledge, and Canyon View.
The printed name of the climbable-cliff is "cliff".
Understand "cliff" and "wall" and "walls" and "climbable" as the climbable-cliff.
The description of the climbable-cliff is "The cliff is steep and rocky."
Instead of climbing the climbable-cliff: say "You can[apostrophe]t do that!"
Instead of jumping when the player can see the climbable-cliff and the player is not in Canyon View:
  say "That would be very unwise. Perhaps even fatal."
Rocky Ledge is a room. The printed name of Rocky Ledge is "Rocky Ledge". "You are on a ledge about halfway up the wall of the river canyon. You can see from here that the main flow from Aragain Falls twists along a passage which it is impossible for you to enter. Below you is the canyon bottom. Above you is more cliff, which appears climbable."
Rocky Ledge is in the Underground.
Up from Rocky Ledge is Canyon View. Down from Rocky Ledge is Canyon Bottom.
Canyon View is a room. "You are at the top of the Great Canyon on its west wall. From here there is a marvelous view of the canyon and parts of the Frigid River upstream. Across the canyon, the walls of the White Cliffs join the mighty ramparts of the Flathead Mountains to the east. Following the Canyon upstream to the north, Aragain Falls may be seen, complete with rainbow. The mighty Frigid River flows out from a great dark cavern. To the west and south can be seen an immense forest, stretching for miles around. A path leads northwest. It is possible to climb down into the canyon from here."
Canyon View is in the Underground.
East of Canyon View is Rocky Ledge. Down from Canyon View is Rocky Ledge.
Northwest of Canyon View is Clearing. West of Canyon View is Forest3.
East of Clearing is Canyon View.
Instead of going south in Canyon View:
  say "Storm-tossed trees block your way."
-}
