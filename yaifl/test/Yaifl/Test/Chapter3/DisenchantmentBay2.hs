module Yaifl.Test.Chapter3.DisenchantmentBay2 where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Game.Create.Object
import Yaifl.Game.EffectHandlers
import Yaifl.Game.ObjectSpecifics
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Kinds.Supporter
import Yaifl.Model.Metadata
import Yaifl.Test.Common
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Query

ex15 :: (Text, [Text], Game PlainWorldModel ())
ex15 = ("Disenchantment Bay 2", disenchantmentBayTestMeWith, disenchantmentBayWorld)

disenchantmentBayWorld :: Game PlainWorldModel ()
disenchantmentBayWorld = do
  setTitle "Disenchantment Bay"
  addRoom "The Cabin" ! #description
    [wrappedText|The front of the small cabin is entirely occupied with navigational instruments,
a radar display, and radios for calling back to shore. Along each side runs a bench with faded blue
vinyl cushions, which can be lifted to reveal the storage space underneath. A glass case against the
wall contains several fishing rods.

Scratched windows offer a view of the surrounding bay, and there is a door south to the deck.
A sign taped to one wall announces the menu of tours offered by the Yakutat Charter Boat Company.|]

  gc <- addContainer "glass case"
    ! #openable Openable
    ! #opacity Transparent
    ! #opened Closed
    ! #modify makeItScenery
    ! done
  addThing "collection of fishing rods"
    ! #location (inThe gc)
    ! done
  b <- addSupporter "bench"
    ! #enterable Enterable
    ! #modify makeItScenery
    ! done
  addThing "blue vinyl cushions"
    ! #modify (makeItScenery >> #namePlurality .= PluralNamed)
    ! #location (onThe b)
    ! done
  mapM_ (\n -> addThing n ! #modify (makeItScenery >> #namePlurality .= PluralNamed) ! done)
    [ "navigational instruments"
    , "scratched windows"
    , "radios"
    ]
  mapM_ (\n -> addThing n ! #modify makeItScenery ! done)
    [ "sign"
    , "radar display"
    ]
  pass

disenchantmentBayTestMeWith :: [Text]
disenchantmentBayTestMeWith = ["examine instruments", "x windows", "x sign", "x display", "x radios"]