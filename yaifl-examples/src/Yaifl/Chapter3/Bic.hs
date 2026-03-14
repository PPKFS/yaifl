module Yaifl.Chapter3.Bic where

import Yaifl.Prelude
import Yaifl

import Yaifl.Effects.ObjectQuery

import Yaifl.Text.Say
import qualified Data.Text as T
import Yaifl.Thing.Kind
import Yaifl.Object.Kind
import Yaifl.Rulebook
import Yaifl.Object.Create
import Yaifl.Room.Create
import Yaifl.Thing.Create

ex2 :: (Text, [a], Game PlainWorldModel ())
ex2 = ("Bic", [], ex2World)

isBlankDescription :: Display (WMText wm) => Thing wm -> Bool
isBlankDescription d = T.empty == display (d ^. #description)

ex2World :: Game PlainWorldModel ()
ex2World = do
  setTitle "Bic"

  addRoom "The Staff Break Room" ! done

  addThing "Bic pen"
    -- because we want "You see a Bic pen" not "You see Bic pen".
    ! #modify makeNameImproper
    ! done

  addThing "orange"
    ! #description "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice."
    ! done

  addThing "napkin"
    ! #description "Slightly crumpled."
    ! done

  addWhenPlayBegins $ makeRule' "run property checks at the start of play rule" $ do
      traverseThings_ (\t -> when (isBlankDescription t) [saying|{t} has no description.|] >> rulePass)
      rulePass
