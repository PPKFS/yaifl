module Yaifl.Test.Chapter3.Bic where

import Yaifl
import Yaifl.Core.Metadata
import Yaifl.Core.Object
import Yaifl.Core.Objects.Create
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Core.Print
import Yaifl.Core.World
import Yaifl.Core.Rules.Rule
import qualified Data.Text as T
import Solitude
import Data.Text.Display
import Yaifl.Core.WorldModel

isBlankDescription :: Display (WMSayable wm) => Thing wm -> Bool
isBlankDescription d = T.empty == display (d ^. #description)

ex2World :: Game PlainWorldModel ()
ex2World = do
  setTitle "Bic"
  addRoom' "The Staff Break Room" "" pass
  addThing' "Bic pen" "" pass
  addThing' "orange" "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice." pass
  addThing' "napkin" "Slightly crumpled." pass
  pass
  addWhenPlayBegins $ makeRule' "run property checks at the start of play" $
    do
      traverseThings (\t -> do
        when (isBlankDescription t) (do
          say t
          printLn " has no description.")
        return Nothing)
      return Nothing