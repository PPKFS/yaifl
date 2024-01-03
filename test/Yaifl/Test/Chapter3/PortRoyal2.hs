module Yaifl.Test.Chapter3.PortRoyal2 where

import Yaifl
import Yaifl.Metadata
import Yaifl.Model.Objects.Create
import Solitude
import Yaifl.Test.Common
import Yaifl.Model.Objects.RoomConnections
import Yaifl.Model.Direction

ex8 :: (Text, [Text], Game PlainWorldModel ())
ex8 = ("Port Royal 2", portRoyal2TestMeWith, portRoyalWorld2)

portRoyalWorld2 :: Game PlainWorldModel ()
portRoyalWorld2 = do
  setTitle "1691"
  ts <- addRoom "Thames Street End" [wrappedText|The ill-named Thames Street runs from here -- at the point of the peninsula --
all the way east among houses and shops, through the Fish Market, edging by the round front of Fort Carlisle, to the point
where the town stops and there is only sandy spit beyond. Lime Street, wider and healthier but not as rich,
runs directly south, and to the north the road opens up into the courtyard of Fort James.|]
  fr <- addRoom "Fisher's Row" [wrappedText|"A waterfront street that runs south towards Chocolata Hole, where the small craft are harboured.
It also continues north around the tip of the peninsula from here, turning into the east-west Thames Street.|]
  fr `isWestOfOneWay` ts
  ts `isNorthOfOneWay` fr

  fj <- addRoom "Fort James" ""
  ts `isBelow` fj
  ts `isSouthOf` fj
  ts `isNowhere` Up

portRoyal2TestMeWith :: [Text]
portRoyal2TestMeWith = ["n", "d", "u", "w", "e", "n", "s"]