module Yaifl.Test.Chapter3.MidsummerDay where

import Yaifl.Prelude
import Yaifl
import Yaifl.Game.Create

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
