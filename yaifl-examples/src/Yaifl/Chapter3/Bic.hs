module Yaifl.Chapter3.Bic where

import Yaifl.Prelude
import Yaifl

import Yaifl.Effects.ObjectQuery

import Yaifl.Text.Say
import qualified Data.Text as T
import Yaifl.Thing.Kind
import Yaifl.Object.Kind
import Yaifl.Rulebook
import Yaifl.Room.Create
import Yaifl.Thing.Create as T
import Yaifl.Combinators
import Yaifl.Effects.RuleEffects

ex2 :: (Text, [a], Game PlainWorldModel ())
ex2 = ("Bic", [], ex2World)

isBlankDescription ::
  SayableValue (WMText wm) wm
  => RuleEffects wm es
  => Thing wm
  -> Eff es Bool
isBlankDescription d = T.null <$> sayText (d ^. #description)

ex2World :: Game PlainWorldModel ()
ex2World = do
  setTitle "Bic"

  addRoom' "The Staff Break Room"

  {-
  This is meant to highlight the 3 ways to create something with some properties:
    1. using predefined combinators (in this case, improperlyNamed).
    2. using record update syntax
    3. using labels and lenses
  -}
  addThing "Bic pen" $ newThing
    -- because we want "You see a Bic pen" not "You see Bic pen".
    & makeThingImproperlyNamed

  addThing "orange" $ newThing
    { T.description = "It's a small hard pinch-skinned thing from the lunch room, probably with lots of pips and no juice."
    }

  addThing "napkin" $ newThing
    & #description .~ "Slightly crumpled."

  whenPlayBegins $ makeRule' "run property checks at the start of play rule" $ do
      traverseThings_ (\t -> whenM (isBlankDescription t) [saying|{t} has no description.|] >> rulePass)
      rulePass
