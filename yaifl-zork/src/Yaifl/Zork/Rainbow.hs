module Yaifl.Zork.Rainbow where

{-
TODO
Chapter 18 - Sceptre and Rainbow
-}

{-
Carry out waving: say "You wave [the noun] around. Nothing happens."
-}

{-
Instead of waving the sceptre:
  if the player is in On-the-Rainbow:
    die saying "The structural integrity of the rainbow is severely compromised, leaving you hanging in midair, supported only by water vapor. Bye.";
-}

{-
  otherwise if the player is in Aragain Falls or the player is in End of Rainbow:
    if the rainbow-flag is false:
      now the rainbow-flag is true;
      now the pot of gold is zil-visible;
      say "Suddenly, the rainbow appears to become solid and, I venture, walkable (I think the giveaway was the stairs and bannister).";
-}

{-
      if the player is in End of Rainbow and the pot of gold is in End of Rainbow:
        say "[line break]A shimmering pot of gold appears at the end of the rainbow.";
-}

{-
    otherwise:
      now the rainbow-flag is false;
      now the pot of gold is zil-invisible;
      say "The rainbow seems to have become somewhat run-of-the-mill.";
-}

{-
  otherwise:
    say "A dazzling display of color briefly emanates from the sceptre."
-}

{-
The rainbow-object is a backdrop. The rainbow-object is in Aragain Falls, On-the-Rainbow, End of Rainbow, and Canyon View. The printed name of the rainbow-object is "rainbow".
Understand "rainbow" as the rainbow-object.
-}

{-
The description of the rainbow-object is "[if the rainbow-flag is true]The rainbow is solid and provides a colorful path across the Falls.[otherwise]The rainbow is a beautiful arc of colors spanning the Falls.[end if]"
-}

{-
Instead of entering the rainbow-object in Canyon View:
  say "From here?!?"
-}

{-
Instead of entering the rainbow-object in On-the-Rainbow:
  say "You[apostrophe]ll have to say which way..."
-}

{-
Instead of entering the rainbow-object:
  if the rainbow-flag is true:
    if the player is in Aragain Falls:
      move the player to On-the-Rainbow;
-}

{-
    otherwise if the player is in End of Rainbow:
      move the player to On-the-Rainbow;
  otherwise:
    say "Can you walk on water vapor?"
-}

{-
Instead of looking under the rainbow-object:
  say "The Frigid River flows under the rainbow."
-}
