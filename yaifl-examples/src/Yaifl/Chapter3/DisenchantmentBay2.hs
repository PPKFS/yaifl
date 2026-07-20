module Yaifl.Chapter3.DisenchantmentBay2 where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Effects.Interpreters
import Yaifl.Container.Kind
import Yaifl.Supporter.Kind
import Yaifl.Metadata
import Yaifl.Test.Common
import Yaifl.Room.Create
import Yaifl.Container.Create
import Yaifl.Supporter.Create
import Yaifl.Thing.Create
import Yaifl.Combinators

ex15 :: (Text, [Text], Game PlainWorldModel ())
ex15 = ("Disenchantment Bay 2", disenchantmentBayTestMeWith, disenchantmentBayWorld)

disenchantmentBayWorld :: Game PlainWorldModel ()
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
    & makeItScenery

  addThing "collection of fishing rods" $ newThing
    & placeIt (inThe glassCase)

  bench <- addSupporter "bench" $ newSupporter
    & makeItEnterable
    & makeItScenery

  addThing "blue vinyl cushions" $ newThing
    & makeItPlural
    & placeIt (onThe bench)
    & makeItScenery

  mapM_ (\n -> addThing n $ newThing & makeItPlural & makeItScenery)
    [ "navigational instruments"
    , "scratched windows"
    , "radios"
    ]
  mapM_ (\n -> addThing n $ newThing & makeItScenery)
    [ "sign"
    , "radar display"
    ]

disenchantmentBayTestMeWith :: [Text]
disenchantmentBayTestMeWith = ["examine instruments", "x windows", "x sign", "x display", "x radios"]
