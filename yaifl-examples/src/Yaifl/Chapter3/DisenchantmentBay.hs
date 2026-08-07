module Yaifl.Chapter3.DisenchantmentBay where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Combinators
import Yaifl.Container.Create
import Yaifl.Container.Kind
import Yaifl.Effects.Interpreters
import Yaifl.Metadata
import Yaifl.Room.Create as R
import Yaifl.Supporter.Create as S
import Yaifl.Supporter.Kind
import Yaifl.Test.Common
import Yaifl.Thing.Create as T

ex14 :: (Text, [Text], WorldConstruction PlainWorldModel ())
ex14 = ("Disenchantment Bay", disenchantmentBayTestMeWith, disenchantmentBayWorld)

disenchantmentBayWorld :: WorldConstruction PlainWorldModel ()
disenchantmentBayWorld = do
  setTitle "Disenchantment Bay"
  addRoom "The Cabin" $ newRoom
    & #description .~ [wrappedText|The front of the small cabin is entirely occupied with navigational instruments,
a radar display, and radios for calling back to shore. Along each side runs a bench with faded blue
vinyl cushions, which can be lifted to reveal the storage space underneath. A glass case against the
wall contains several fishing rods.

Scratched windows offer a view of the surrounding bay, and there is a door south to the deck.
A sign taped to one wall announces the menu of tours offered by the Yakutat Charter Boat Company.|]

  glassCase <- addContainer "glass case" $ newContainer
    & makeItClosedAndOpenable
    & makeItTransparent

  addThing "collection of fishing rods" $ newThing
    & placeIt (inThe glassCase)

  bench <- addSupporter "bench" $ newSupporter
    & makeItEnterable

  addThing "blue vinyl cushions" $ newThing
    & makeItPlural
    & placeIt (onThe bench)
  pass

disenchantmentBayTestMeWith :: [Text]
disenchantmentBayTestMeWith = ["examine case", "get rods", "open case", "get rods", "sit on bench", "take cushions", "get up"]
