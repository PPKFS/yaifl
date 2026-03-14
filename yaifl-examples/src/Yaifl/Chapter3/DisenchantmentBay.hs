module Yaifl.Chapter3.DisenchantmentBay where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Object.Create
import Yaifl.Effects.Interpreters
import Yaifl.Container.Kind
import Yaifl.Openable.Kind
import Yaifl.Supporter.Kind
import Yaifl.Metadata
import Yaifl.Test.Common
import Yaifl.Object.Kind
import Yaifl.Room.Create
import Yaifl.Container.Create
import Yaifl.Supporter.Create
import Yaifl.Thing.Create

ex14 :: (Text, [Text], Game PlainWorldModel ())
ex14 = ("Disenchantment Bay", disenchantmentBayTestMeWith, disenchantmentBayWorld)

disenchantmentBayWorld :: Game PlainWorldModel ()
disenchantmentBayWorld = do
  setTitle "Disenchantment Bay"
  addRoom "The Cabin"
    ! #description [wrappedText|The front of the small cabin is entirely occupied with navigational instruments,
a radar display, and radios for calling back to shore. Along each side runs a bench with faded blue
vinyl cushions, which can be lifted to reveal the storage space underneath. A glass case against the
wall contains several fishing rods.

Scratched windows offer a view of the surrounding bay, and there is a door south to the deck.
A sign taped to one wall announces the menu of tours offered by the Yakutat Charter Boat Company.|]
    ! done

  gc <- addContainer "glass case"
    ! #openable Openable
    ! #opacity Transparent
    ! #opened Closed
    ! done
  addThing "collection of fishing rods"
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
