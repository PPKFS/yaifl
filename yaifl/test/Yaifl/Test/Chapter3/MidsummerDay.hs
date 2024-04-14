module Yaifl.Test.Chapter3.MidsummerDay where

import Solitude
import Yaifl.Game.EffectHandlers
import Yaifl (PlainWorldModel)
import Yaifl.Model.Metadata
import Yaifl.Test.Common
import Yaifl.Game.Create.Object
import Named
import Yaifl.Game.Create.RoomConnection
import Yaifl.Game.ObjectSpecifics
import Yaifl.Model.Kinds.Supporter
import Yaifl.Model.Kinds.Container

ex12 :: (Text, [Text], Game PlainWorldModel ())
ex12 = ("Midsummer Day", midsummerDayTestMeWith, midsummerDayWorld)

midsummerDayWorld :: Game PlainWorldModel ()
midsummerDayWorld = do
  setTitle "Midsummer Day"
  ga <- addRoom "Garden" ! done
  gz <- addRoom "Gazebo" ! done
  gz `isEastOf` ga
  bt <- addSupporter "billiards table" ! done
  tc <- addContainer "trophy cup"
    ! #location (onThe bt)
    ! done
  addThing "starting pistol"
    ! #location (inThe tc)
    ! done
  th <- addRoom "Treehouse" ! done
  th `isAbove` gz
  addContainer "cardboard box" ! done
  pass

midsummerDayTestMeWith :: [Text]
midsummerDayTestMeWith = ["up", "x box", "d", "e", "x table", "x cup", "x pistol", "get cup"]
