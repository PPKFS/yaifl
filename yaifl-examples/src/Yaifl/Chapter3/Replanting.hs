module Yaifl.Chapter3.Replanting where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Object.Create
import Yaifl.Effects.Interpreters
import Yaifl.Metadata
import Yaifl.Room.Create
import Yaifl.Thing.Create
import Yaifl.Thing.Kind ( thingIsScenery )
import Yaifl.Text.SayableValue
import Yaifl.Rulebook
import Yaifl.Actions.Imports
import Yaifl.Create.Rule

ex16 :: (Text, [Text], Game PlainWorldModel ())
ex16 = ("Replanting", replantingTestMeWith, replantingWorld)

replantingWorld :: Game PlainWorldModel ()
replantingWorld = do
  setTitle "Replanting"
  addRoom "The Orchard"
    ! #description "Within this quadrille of pear trees, a single gnarled old oak remains as a memory of centuries past."
    ! done

  addThing "gnarled oak tree"
    ! #modify makeItScenery
    ! done
  insteadOf #taking [Precondition (pure "taking scenery") $
    \t -> return (thingIsScenery (variables t))] (const $ say @Text "You lack the hulk-like strength.")
  pass

replantingTestMeWith :: [Text]
replantingTestMeWith = ["take oak"]
