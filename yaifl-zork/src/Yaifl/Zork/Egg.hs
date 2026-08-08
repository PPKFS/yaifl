module Yaifl.Zork.Egg where

{-
TODO
Section 5 - Objects in the Tree
The bird's nest is in Up a Tree. "Beside you on the branch is a small bird's nest."
Understand "nest" and "bird's" as the bird's nest.
The bird's nest is an open container. The carrying capacity of the bird's nest is 3.
The description of the bird's nest is "The bird's nest is a rough collection of twigs and grass."
The jewel-encrusted egg is in the bird's nest. "In the bird's nest is a large egg encrusted with precious jewels, apparently scavenged by a childless songbird. The egg is covered with fine gold inlay, and ornamented in lapis lazuli and mother-of-pearl. Unlike most eggs, this one is hinged and closed with a delicate looking clasp. The egg appears extremely fragile."
Rule for writing a paragraph about the bird's nest:
  say "Beside you on the branch is a small bird's nest.[line break]";
  if the jewel-encrusted egg is in the bird's nest and the jewel-encrusted egg is not handled:
    say "In the bird's nest is a large egg encrusted with precious jewels, apparently scavenged by a childless songbird. The egg is covered with fine gold inlay, and ornamented in lapis lazuli and mother-of-pearl. Unlike most eggs, this one is hinged and closed with a delicate looking clasp. The egg appears extremely fragile.[line break]";
    now the jewel-encrusted egg is mentioned.
Understand "egg" and "jewel" and "encrusted" and "jeweled" and "bird's" as the jewel-encrusted egg.
The jewel-encrusted egg is a closed openable container. The carrying capacity of the jewel-encrusted egg is 1.
The treasure-value of the jewel-encrusted egg is 5.
The point-value of the jewel-encrusted egg is 5.
Rule for writing a paragraph about the jewel-encrusted egg when the jewel-encrusted egg is open:
  if the golden clockwork canary is in the jewel-encrusted egg and the golden clockwork canary is not handled:
    say "There is a golden clockwork canary nestled in the egg. It has ruby eyes and a silver beak. Through a crystal window below its left wing you can see intricate machinery inside. It appears to have wound down.[line break]";
    now the golden clockwork canary is mentioned.
The golden clockwork canary is in the jewel-encrusted egg. "There is a golden clockwork canary nestled in the egg. It has ruby eyes and a silver beak. Through a crystal window below its left wing you can see intricate machinery inside. It appears to have wound down."
Understand "canary" and "clockwork" and "gold" and "golden" as the golden clockwork canary.
The treasure-value of the golden clockwork canary is 4.
The point-value of the golden clockwork canary is 6.
The description of the golden clockwork canary is "The canary is a beautiful golden clockwork device. It appears to have wound down."
The broken jewel-encrusted egg is a thing. The printed name of the broken jewel-encrusted egg is "broken jewel-encrusted egg". "There is a somewhat ruined egg here."
Understand "broken" and "egg" and "jewel" and "encrusted" as the broken jewel-encrusted egg.
The broken jewel-encrusted egg is an open container. The carrying capacity of the broken jewel-encrusted egg is 1.
The treasure-value of the broken jewel-encrusted egg is 2.
The broken clockwork canary is a thing. The printed name of the broken clockwork canary is "broken clockwork canary". "There is a golden clockwork canary nestled in the egg. It seems to have recently had a bad experience. The mountings for its jewel-like eyes are empty, and its silver beak is crumpled. Through a cracked crystal window below its left wing you can see the remains of intricate machinery. It is not clear what result winding it would have, as the mainspring seems sprung."
Understand "broken" and "canary" and "clockwork" and "gold" and "golden" as the broken clockwork canary.
The treasure-value of the broken clockwork canary is 1.
The beautiful brass bauble is a thing. "A beautiful brass bauble is here."
Understand "bauble" and "brass" and "beautiful" as the beautiful brass bauble.
The treasure-value of the beautiful brass bauble is 1.
The point-value of the beautiful brass bauble is 1.
Section 6 - Egg Fragility
The egg-broken is a truth state that varies. The egg-broken is false.
To break-the-egg:
  if the egg-broken is true, stop;
  now the egg-broken is true;
  if the golden clockwork canary is in the jewel-encrusted egg:
    say " There is a golden clockwork canary nestled in the egg. It seems to have recently had a bad experience. The mountings for its jewel-like eyes are empty, and its silver beak is crumpled. Through a cracked crystal window below its left wing you can see the remains of intricate machinery. It is not clear what result winding it would have, as the mainspring seems sprung.";
    now the broken clockwork canary is in the broken jewel-encrusted egg;
    remove the golden clockwork canary from play;
  otherwise:
    remove the broken clockwork canary from play;
  now the broken jewel-encrusted egg is in the holder of the jewel-encrusted egg;
  remove the jewel-encrusted egg from play.
Instead of opening the jewel-encrusted egg:
  if the jewel-encrusted egg is open:
    say "The egg is already open." instead;
  say "You have neither the tools nor the expertise."
Prying open it with is an action applying to two things. Understand "open [something] with [something]" as prying open it with.
Instead of prying open the jewel-encrusted egg with something:
  if the jewel-encrusted egg is open:
    say "The egg is already open." instead;
  if the second noun is a weapon:
    say "The egg is now open, but the clumsiness of your attempt has seriously compromised its esthetic appeal.";
    break-the-egg;
  otherwise:
    say "The concept of using a [second noun] is certainly original."
A thing can be a weapon or not a weapon. A thing is usually not a weapon.
The sword is a weapon. The nasty knife is a weapon.
Instead of entering the jewel-encrusted egg:
  say "There is a noticeable crunch from beneath you, and inspection reveals that the egg is lying open, badly damaged.";
  break-the-egg.
Instead of attacking the jewel-encrusted egg:
  say "Your rather indelicate handling of the egg has caused it some damage, although you have succeeded in opening it.";
  break-the-egg.
After dropping the jewel-encrusted egg in Up a Tree:
  say "The egg falls to the ground and springs open, seriously damaged.";
  now the jewel-encrusted egg is in Forest Path;
  break-the-egg.
After dropping the bird's nest in Up a Tree:
  if the jewel-encrusted egg is in the bird's nest:
    say "The nest falls to the ground, and the egg spills out of it, seriously damaged.";
    now the bird's nest is in Forest Path;
    now the jewel-encrusted egg is in Forest Path;
    break-the-egg;
  otherwise:
    say "The bird's nest falls to the ground.";
    now the bird's nest is in Forest Path.
Section 7 - Canary Wind-up and Bauble
The canary-sang is a truth state that varies. The canary-sang is false.
Winding is an action applying to one thing. Understand "wind [something]" and "wind up [something]" as winding.
Carry out winding: say "You cannot wind up a [noun]."
Instead of winding the golden clockwork canary:
  if the canary-sang is false and the player is in the Forest Area:
    now the canary-sang is true;
    say "The canary chirps, slightly off-key, an aria from a forgotten opera. From out of the greenery flies a lovely songbird. It perches on a limb just over your head and opens its beak to sing. As it does so a beautiful brass bauble drops from its mouth, bounces off the top of your head, and lands glimmering in the grass. As the canary winds down, the songbird flies away.";
    if the player is in Up a Tree:
      now the beautiful brass bauble is in Forest Path;
    otherwise:
      now the beautiful brass bauble is in the location of the player;
  otherwise:
    say "The canary chirps blithely, if somewhat tinnily, for a short time."
Instead of winding the broken clockwork canary:
  say "There is an unpleasant grinding noise from inside the canary."
-}