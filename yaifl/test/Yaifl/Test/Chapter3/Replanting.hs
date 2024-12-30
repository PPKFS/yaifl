module Yaifl.Test.Chapter3.Replanting where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Game.Create.Object
import Yaifl.Game.EffectHandlers
import Yaifl.Game.ObjectSpecifics
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Kinds.Supporter
import Yaifl.Core.Metadata
import Yaifl.Test.Common
import Yaifl.Core.Kinds.Object
import Yaifl.Model.Query
import Yaifl.Game.Create
import Yaifl.Model.Rules (Precondition(..), variables, say)
import Yaifl.Core.Kinds.Thing ( thingIsScenery )

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
