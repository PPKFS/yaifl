module Yaifl.Test.Chapter3.Bic where

import Yaifl

import Yaifl.Core.Objects.Create
--import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Test.Common
--import Yaifl.Core.Activities.PrintingNameOfSomething
--import Yaifl.Core.Rulebooks.WhenPlayBegins
import qualified Data.Text as T
import Yaifl.Core.Common
import Yaifl.Core.World
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Objects.Query
import Yaifl.Lamp.Activities.PrintingNameOfSomething
import Yaifl.Core.Say
import Yaifl.Core.Objects.Object
import Cleff.State

isBlankDescription :: State Metadata :> es => Thing wm -> Eff es Bool
isBlankDescription d = (T.empty ==) <$> evalDesc d

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
        whenM (isBlankDescription t) (do
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