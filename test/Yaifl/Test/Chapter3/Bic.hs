module Yaifl.Test.Chapter3.Bic where

import Yaifl
import Yaifl.Core.Metadata
import Yaifl.Core.Object
import Yaifl.Core.Objects.Create
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Say
import Yaifl.Core.World
import Yaifl.Lamp.Activities.PrintingNameOfSomething
import Yaifl.Test.Common
import qualified Data.Text as T
import Solitude
import Yaifl.Core.AdaptiveText

isBlankDescription :: Thing wm -> Bool
isBlankDescription d = T.empty == rawAdaptiveText (d ^. objDescription)

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
          printName t
          sayLn " has no description.")
        return Nothing)
      return Nothing

ex2Test :: [Text]
ex2Test =
  [ expectLooking "The Staff Break Room" ""
  , expectYouCanSee ["a Bic pen", "a orange", "a napkin"]
  , expectLine "Bic pen has no description."
  ]