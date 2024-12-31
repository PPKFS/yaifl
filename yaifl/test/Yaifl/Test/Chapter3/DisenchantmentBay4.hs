module Yaifl.Test.Chapter3.DisenchantmentBay4 where

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
import Yaifl.Core.Effects (traverseRooms)
import Yaifl.Core.Tag
import Yaifl.Core.Kinds.Room
import qualified Data.List.NonEmpty as NE

ex18 :: (Text, [Text], Game PlainWorldModel ())
ex18 = ("Disenchantment Bay 4", disenchantmentBayTestMeWith, disenchantmentBayWorld)

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
  mapM_ (\(n, d) -> addThing n ! #description d ! #modify (makeItScenery >> #namePlurality .= PluralNamed) ! done)
    [ ("navigational instruments", "Knowing what they do is the Captain's job.")
    , ("scratched windows", "They're a bit the worse for wear, but you can still get an impressive view of the glacier through them. There were whales earlier, but they're gone now.")
    , ("radios", "With any luck you will not need to radio for help, but it is reassuring that these things are here.")
    ]
  mapM_ (\(n, d) -> addThing n ! #description d ! #modify makeItScenery ! done)
    [ ("sign", "You can get half-day and full-day sight-seeing tours, and half-day and full-day fishing trips.")
    , ("radar display", "Apparently necessary to avoid the larger icebergs.")
    ]
  allRooms <- traverseRooms (const (return Nothing))
  case allRooms of
    [] -> error "impossible; traverseRooms is broken"
    r:rs -> addBackdrop "view of the Malaspina glacier" ! #description "The Malaspina glacier covers much of the nearby slope, and -- beyond it -- an area as large as Rhode Island."
              ! #locations (NE.map (coerceTag . tagRoomEntity) $ r :| rs) ! done

  pass

disenchantmentBayTestMeWith :: [Text]
disenchantmentBayTestMeWith = fromI7TestMe "examine sign / examine glacier / examine instruments / examine windows / examine radar / examine radios / take the cushions / take the glacier"