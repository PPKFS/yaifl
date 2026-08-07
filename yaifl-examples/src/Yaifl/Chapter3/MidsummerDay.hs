module Yaifl.Chapter3.MidsummerDay where

import Yaifl.Prelude
import Yaifl
import Yaifl.Supporter.Kind
import Yaifl.Container.Kind
import Yaifl.Room.Create
import Yaifl.Thing.Create
import Yaifl.Supporter.Create
import Yaifl.Room.Query
import Yaifl.Container.Create
import Yaifl.Combinators
import Yaifl.Effects.Interpreters

ex12 :: (Text, [Text], WorldConstruction PlainWorldModel ())
ex12 = ("Midsummer Day", midsummerDayTestMeWith, midsummerDayWorld)

midsummerDayWorld :: WorldConstruction PlainWorldModel ()
midsummerDayWorld = do
  setTitle "Midsummer Day"
  ga <- addRoom' "Garden"
  gz <- addRoom' "Gazebo"
  gz `isEastOf` ga
  bt <- addSupporter "billiards table" newSupporter
  tc <- addContainer "trophy cup" $ newContainer
    & placeIt (onThe bt)
  addThing "starting pistol" $ newThing
    & placeIt (inThe tc)
  th <- addRoom' "Treehouse"
  th `isAbove` ga
  addContainer "cardboard box" newContainer
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
