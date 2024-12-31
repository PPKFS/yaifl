module Yaifl.Test.Chapter3.Replanting where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Game.Create.Object
import Yaifl.Game.EffectHandlers
import Yaifl.Core.Metadata
import Yaifl.Model.Query
import Yaifl.Game.Create
import Yaifl.Core.Kinds.Thing ( thingIsScenery )
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Say

ex16 :: (Text, [Text], Game PlainWorldModel ())
ex16 = ("Replanting", replantingTestMeWith, replantingWorld)

replantingWorld :: Game PlainWorldModel ()
replantingWorld = do
  setTitle "Replanting"
  addRoom "The Orchard" ! #description
    "Within this quadrille of pear trees, a single gnarled old oak remains as a memory of centuries past."

  addThing "gnarled oak tree"
    ! #modify makeItScenery
    ! done
  insteadOf (ActionRule #taking) [Precondition (pure "taking scenery") $
    \t -> return (thingIsScenery (variables t))] (const $ say @Text "You lack the hulk-like strength.")
  pass

replantingTestMeWith :: [Text]
replantingTestMeWith = ["take oak"]
