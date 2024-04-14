module Yaifl.Test.Chapter3.Bic where

import Yaifl
import Yaifl.Game.Create.Object
import Yaifl.Model.Effects
import Yaifl.Model.Kinds
import Yaifl.Model.Rules
import Yaifl.Prelude
import Yaifl.Text.Say
import qualified Data.Text as T

ex2 :: (Text, [a], Game PlainWorldModel ())
ex2 = ("Bic", [], ex2World)

isBlankDescription :: Display (WMSayable wm) => Thing wm -> Bool
isBlankDescription d = T.empty == display (d ^. #description)

ex2World :: Game PlainWorldModel ()
ex2World = do
  setTitle "Bic"
  addRoom "The Staff Break Room"
    ! done

  addThing "Bic pen"
    ! #modify (#nameProperness .= Improper)
    ! done

  addThing "orange"
    ! #description "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice."
    ! done

  addThing "napkin"
    ! #description "Slightly crumpled."
    ! done

  addWhenPlayBegins $ makeRule' "run property checks at the start of play rule" $ do
      traverseThings (\t -> when (isBlankDescription t) [saying|{t} has no description.|] >> return Nothing)
      rulePass
