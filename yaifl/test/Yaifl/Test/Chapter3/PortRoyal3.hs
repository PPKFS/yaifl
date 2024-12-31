module Yaifl.Test.Chapter3.PortRoyal3 where

import Yaifl.Prelude
import Yaifl
import Yaifl.Game.Create
import Yaifl.Core.Query.Region
import Yaifl.Model.Kinds.Direction
import Yaifl.Test.Common

ex10 :: (Text, [Text], Game PlainWorldModel ())
ex10 = ("Port Royal 3", portRoyal3TestMeWith, portRoyal3World)

-- a combination of port royale:
-- part 1 https://ganelson.github.io/inform-website/book/WI_3_2.html
portRoyal3World :: Game PlainWorldModel ()
portRoyal3World = do
  setTitle "1691"
  fj <- addRoom "Fort James" ! #description [wrappedText|The enclosure of Fort James is a large, roughly hexagonal court walled with heavy stone.
The walls face the entrance to Port Royal Harbour, and the battery of guns is prepared to destroy any enemy ship arriving.|]

  ts <- addRoom "Thames Street End" ! #description [wrappedText|The ill-named Thames Street runs from here -- at the point of the peninsula -- all the way east among houses and shops,
through the Fish Market, edging by the round front of Fort Carlisle, to the point where the town stops and there is only sandy spit beyond. Most of that stretch is
full of people at all hours. Imported goods are moved off of ships and taken to distributors; exported goods are brought to be loaded; and there is one public house and brothel for every ten inhabitants.

Lime Street, wider and healthier but not as rich, runs directly south, and to the north the road opens up into the courtyard of Fort James.|]
  ts `isSouthOf` fj

  ls <- addRoom "Lime Street" ! done
  ls `isSouthOf` ts
  fr <- addRoom "Fisher's Row" ! #description [wrappedText|"A waterfront street that runs south towards Chocolata Hole, where the small craft are harboured.
It also continues north around the tip of the peninsula from here, turning into the east-west Thames Street.|]

  ts `isBelow` fj
  ts `isNowhere` Up
  fr `isWestOfOneWay` ts
  ts `isNorthOfOneWay` fr

  wl <- addRoom "Water Lane" ! #description [wrappedText|Here Thames Street -- never very straight -- goes steeply southeast for a portion
before continuing more directly to the east.

Water Lane runs south toward Queen Street, and facing onto it is the New Prison -- which, in the way of these things,
is neither. It did serve in that capacity for a time, and in a measure of the villainy which has been usual in
Port Royal from its earliest days, it is nearly the largest building in the town.|]
  wl `isEastOf` ts

  tsawb <- addRoom "Thames Street at Wherry Bridge" ! #description
    "To the southwest is the fishmarket; directly across the street is the entrance to a private alley through a brick archway."
  tsawb `isEastOf` wl

  tfm <- addRoom "The Fishmarket" ! done
  tfm `isSouthWestOf` tsawb

  tpa <- addRoom "The Private Alley" ! #description [wrappedText|You're just outside the tavern the Feathers. To the north, under a
pretty little archway, is the active mayhem of Thames Street, but the alley narrows down to a dead end a
little distance to the south.|]
  tpa `isSouthOf` tsawb

  tf <- addRoom "The Feathers" ! #description [wrappedText|Newly built with brick, replacing the older Feathers tavern that used to stand here.
It sells wines in quantity, as well as serving them directly, and the goods are always of the best quality.
There's a room upstairs for those wanting to stay the night.|]
  tf `isInsideFrom` tpa

  tsbtkh <- addRoom "Thames Street by the King's House" ! #description [wrappedText|The King's House is reserved for the use of the Governor, but he does not live in it,
and it is frequently being rented out to some merchant so that the government will at least derive some value from it. It is nearly the least interesting
establishment on Thames Street, and the crowd -- which, to the west, is extremely dense -- here thins out a bit.|]
  tsbtkh `isEastOf` tsawb

  tsbfc <- addRoom "Thames Street before Fort Carlisle" ! #description [wrappedText|Here Thames Street, formerly a respectable width, narrows to a footpath in order to
edge around the front of Fort Carlisle, underneath the mouths of the cannon.

There are no buildings on the harbour side of Thames Street at this point, which means that you have an unusually good view of the ships at dock,
water beyond, and the Blue Mountains rising on the other side of the harbour.|]
  tsbfc `isEastOf` tsbtkh
  fc <- addRoom "Fort Carlisle" ! #description [wrappedText|Handsomely arrayed with cannons which you could fire at any moment -- though of course
there are ships at dock which might be in the way.|]
  fc `isSouthOf` tsbfc
  tfb <- addRoom "The Feathers Bedroom" ! done
  tfb `isAbove` tf

  qsm <- addRoom "Queen Street Middle" ! done
  qsen <- addRoom "Queen Street End" ! done
  qse <- addRoom "Queen Street East" ! done
  qsatp <- addRoom "Queen Street at the Prison" ! done
  qse `isSouthOf` ls
  qse `isEastOf` qsm
  qse `isSouthOf` tpa
  qsm `isEastOf` qsen
  qsatp `isEastOf` qse

  i <- addRegion "Inland"
  [qsen, qsm, qse, tpa, ls, qsatp] `areInRegion` i
  wf <- addRegion "Waterfront"
  [tsbtkh, tsbfc, tsawb, wl, tfm, fr, ts] `areInRegion` wf
  mh <- addRegion "Military Holdings"
  [fc, fj] `areInRegion` mh
  t <- addRegion "Tavern"
  t `isSubregionOf` i
  [tf, tfb] `areInRegion` t

portRoyal3TestMeWith :: [Text]
portRoyal3TestMeWith = []