module Yaifl.Test.Chapter3.Bic where

import Yaifl
import Solitude
{-
ex2World :: Game () (World ())

ex2World = newWorld $ do
  setTitle "Bic"
  addRoom' "The Staff Break Room" "" pass
  addThing' "Bic pen" "" pass
  addThing' "orange" "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice." pass
  addThing' "napkin" "Slightly crumpled." pass
  addWhenPlayBegins $ makeRule' "run property checks at the start of play" $
    do
      foreachObject things (\t -> do
        when (isBlankDescription (_objDescription t)) (do
          printName t
          sayLn " has no description.")
        return Nothing)
      return Nothing
-}
ex2Test :: [Text]
ex2Test = []
{-
  [ expectLooking "The Staff Break Room" ""
  , expectYouCanSee ["a Bic pen", "a orange", "a napkin"]
  , expectLine "Bic pen has no description."
  ]-}