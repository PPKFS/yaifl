module Yaifl.Test.Chapter3.PortRoyal where

import Yaifl
import Yaifl.Model.Metadata
import Yaifl.Game.Create.Object
import Solitude
import Yaifl.Test.Common
import Yaifl.Game.Create.RoomConnection
import Named

ex5 :: (Text, [Text], Game PlainWorldModel ())
ex5 = ("Port Royal", portRoyalTestMeWith, portRoyalWorld)

-- a combination of port royale:
-- part 1 https://ganelson.github.io/inform-website/book/WI_3_2.html
portRoyalWorld :: Game PlainWorldModel ()
portRoyalWorld = do
  setTitle "1691"
  fj <- addRoom "Fort James" ! #description [wrappedText|The enclosure of Fort James is a large, roughly hexagonal court walled with heavy stone.
The walls face the entrance to Port Royal Harbour, and the battery of guns is prepared to destroy any enemy ship arriving.|]

  ts <- addRoom "Thames Street End" ! #description [wrappedText|The ill-named Thames Street runs from here -- at the point of the peninsula --
all the way east among houses and shops, through the Fish Market, edging by the round front of Fort Carlisle, to the point
where the town stops and there is only sandy spit beyond. Lime Street, wider and healthier but not as rich,
runs directly south, and to the north the road opens up into the courtyard of Fort James.|]
  ts `isSouthOf` fj

  wl <- addRoom "Water Lane" ! #description [wrappedText|Here Thames Street -- never very straight -- goes steeply southeast for a portion
before continuing more directly to the east.

Water Lane runs south toward Queen Street, and facing onto it is the New Prison -- which, in the way of these things,
is neither. It did serve in that capacity for a time, and in a measure of the villainy which has been usual in
Port Royal from its earliest days, it is nearly the largest building in the town.|]
  wl `isEastOf` ts

  tsawb <- addRoom "Thames Street at Wherry Bridge" ! #description
    "To the southwest is the fishmarket; directly across the street is the entrance to a private alley through a brick archway."
  tsawb `isEastOf` wl

  tpa <- addRoom "The Private Alley" ! #description [wrappedText|You're just outside the tavern the Feathers. To the north, under a
pretty little archway, is the active mayhem of Thames Street, but the alley narrows down to a dead end a
little distance to the south.|]
  tpa `isSouthOf` tsawb

  tf <- addRoom "The Feathers" ! #description [wrappedText|Newly built with brick, replacing the older Feathers tavern that used to stand here.
It sells wines in quantity, as well as serving them directly, and the goods are always of the best quality.
There's a room upstairs for those wanting to stay the night.|]
  tf `isInsideFrom` tpa

  tfb <- addRoom "The Feathers Bedroom" ! done
  tfb `isAbove` tf

  ls <- addRoom "Lime Street" ! done
  ls `isSouthOf` ts

  qsm <- addRoom "Queen Street Middle" ! done

  qse <- addRoom "Queen Street East" ! done
  qse `isEastOf` qsm
  qse `isSouthOf` tpa

  pass

portRoyalTestMeWith :: [Text]
portRoyalTestMeWith = ["s", "e", "e", "s", "in"]