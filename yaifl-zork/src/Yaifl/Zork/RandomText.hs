module Yaifl.Zork.RandomText where

{-
TODO
Section 4b - Random Text Tables
-}

{-
[ZIL YUKS table - used for non-takeable objects and other futile actions]
To say yuks:
-}

{-
  let R be a random number between 1 and 3;
  if R is 1:
    say "A valiant attempt.";
-}

{-
  otherwise if R is 2:
    say "You can[apostrophe]t be serious.";
  otherwise:
    say "An interesting idea..."
-}

{-
[ZIL DUMMY table - used for already-open/already-closed/already-done responses]
To say dummy:
-}

{-
  let R be a random number between 1 and 3;
  if R is 1:
    say "Look around.";
-}

{-
  otherwise if R is 2:
    say "Too late for that.";
  otherwise:
    say "Have your eyes checked."
-}

{-
[ZIL JUMPLOSS table - random death messages for fatal jumps]
To say jumploss:
-}

{-
  let R be a random number between 1 and 3;
  if R is 1:
    say "You should have looked before you leaped.";
-}

{-
  otherwise if R is 2:
    say "In the movies, your life would be passing before your eyes.";
  otherwise:
    say "Geronimo..."
-}

{-
[ZIL uses rotating HO-HUM suffixes for several verbs]
To say ho-hum:
-}

{-
  let R be a random number between 1 and 3;
  if R is 1:
    say "doesn[apostrophe]t seem to work.";
-}

{-
  otherwise if R is 2:
    say "isn[apostrophe]t notably helpful.";
  otherwise:
    say "has no effect."
-}
