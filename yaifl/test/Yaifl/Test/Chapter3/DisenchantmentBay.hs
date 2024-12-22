module Yaifl.Test.Chapter3.DisenchantmentBay where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Game.Create.Object
import Yaifl.Game.EffectHandlers
import Yaifl.Game.ObjectSpecifics
import Yaifl.Model.Kinds (NamePlurality (..))
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Kinds.Supporter
import Yaifl.Model.Metadata
import Yaifl.Test.Common
import Yaifl.Model.Kinds.Object

ex14 :: (Text, [Text], Game PlainWorldModel ())
ex14 = ("Disenchantment Bay", disenchantmentBayTestMeWith, disenchantmentBayWorld)

disenchantmentBayWorld :: Game PlainWorldModel ()
disenchantmentBayWorld = do
  setTitle "Disenchantment Bay"
  _tc <- addRoom "The Cabin" ! #description
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
    ! done
  _fr <- addThing "collection of fishing rods"
    ! #location (inThe gc)
    ! done
  b <- addSupporter "bench"
    ! #enterable Enterable
    ! done
  addThing "blue vinyl cushions"
    ! #modify (#namePlurality .= PluralNamed)
    ! #location (onThe b)
    ! done
  pass

disenchantmentBayTestMeWith :: [Text]
disenchantmentBayTestMeWith = ["examine case", "get rods", "open case", "get rods", "sit on bench", "take cushions", "get up"]
