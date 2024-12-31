module Yaifl.Test.Chapter3.Bic where

import Yaifl
import Yaifl.Game.Create.Object
import Yaifl.Core.Effects
import Yaifl.Prelude
import Yaifl.Text.Say
import qualified Data.Text as T
import Yaifl.Core.Kinds.Thing
import Yaifl.Model.Rules.Rulebook
import Yaifl.Core.Kinds.Object

ex2 :: (Text, [a], Game PlainWorldModel ())
ex2 = ("Bic", [], ex2World)

isBlankDescription :: Display (WMText wm) => Thing wm -> Bool
isBlankDescription d = T.empty == display (d ^. #description)

ex2World :: Game PlainWorldModel ()
ex2World = do
  setTitle "Bic"

  addRoom "The Staff Break Room"
    ! done

  addThing "Bic pen"
    -- because we want "You see a Bic pen" not "You see Bic pen".
    ! #modify (#nameProperness .= Improper)
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
