module Yaifl.Test.Chapter3.Bic where

import Yaifl
import Yaifl.Metadata
import Yaifl.Model.Object
import Yaifl.Model.Objects.Create
import Yaifl.Model.Objects.Effects
import Yaifl.Rules.RuleEffects
import Yaifl.Text.Print
import Yaifl.World
import Yaifl.Rules.Rule
import qualified Data.Text as T
import Solitude
import Data.Text.Display
import Yaifl.Model.WorldModel
import Named
import Yaifl.Text.SayQQ

isBlankDescription :: Display (WMSayable wm) => Thing wm -> Bool
isBlankDescription d = T.empty == display (d ^. #description)

ex2World :: Game PlainWorldModel ()
ex2World = do
  setTitle "Bic"
  addRoom "The Staff Break Room" ""
  addThing "Bic pen" ! defaults
  addThing "orange" ! #description "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice." ! defaults
  addThing "napkin" ! #description "Slightly crumpled." ! defaults
  addWhenPlayBegins $ makeRule' "run property checks at the start of play rule" $
    do
      traverseThings (\t -> do
        when (isBlankDescription t) (do
          [saying|{t} has no description.|]
          )
        return Nothing)
      return Nothing