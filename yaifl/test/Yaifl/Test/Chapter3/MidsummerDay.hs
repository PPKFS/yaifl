module Yaifl.Test.Chapter3.MidsummerDay where

import Yaifl.Prelude
import Yaifl
import Yaifl.Std.Create
import Yaifl.Std.Kinds.Supporter
import Yaifl.Std.Kinds.Container

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
  th `isAbove` ga
  addContainer "cardboard box" ! done
  pass

midsummerDayTestMeWith :: [Text]
midsummerDayTestMeWith =
  [ "up"
  -- You can see a cardboard box (empty) here.
  , "x box"
  -- The cardboard box is empty. TODO
  , "d"
  -- nothing
  , "e"
  -- You can see a billiards table (on which is a trophy cup (in which is a starting pistol)) here.
  , "x table"
  -- On the billiards table is a trophy cup (in which is a starting pistol). TODO
  , "x cup"
  -- In the trophy cup is a starting pistol. TODO, also needs to be visible
  , "x pistol"
  -- You see nothing special about the starting pistol. TODO, also needs to be visible
  , "get cup"
  -- Taken. TODO, also needs to be visible
  ]
