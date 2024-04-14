module Yaifl.Test.Chapter3.Bic where

import Data.Text.Display
import Effectful.Optics ((.=))
import Named
import Solitude
import Yaifl
import Yaifl.Game.Create.Object
import Yaifl.Game.World
import Yaifl.Model.Effects
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Metadata
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.WorldModel
import Yaifl.Text.SayQQ
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
