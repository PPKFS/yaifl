{-# LANGUAGE RecordWildCards #-}
module Yaifl.Door.Create
  ( addDoor
  , newDoor
  , DoorConfig(..)
  ) where

import Yaifl.Prelude

import Yaifl.ObjectSpecifics
import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.Door.Kind
import Yaifl.Tag
import Yaifl.WorldModel
import Yaifl.Thing.Create
import Yaifl.MultiLocated.Query
import Yaifl.Room.Query
import GHC.TypeLits
import Yaifl.Openable.Kind

--TODO: I am only going to enforce implications as a quickcheck or hedgehog
-- invariant, namely that the door type will have a smart ctr (addDoor) that makes sure
-- it isn't portable on creation, and then the check will be whenever we modify an object
-- make sure that it isn't breaking an invariant.

data Purpose
  = Defaults
  | Complete
  deriving stock (Show)

newtype RequiredParameter (a :: Symbol) = RP ()

type family Required (fieldDesc :: Symbol)  (p :: Purpose) a where
  Required f 'Defaults a = RequiredParameter f
  Required f 'Complete a = a

type RequiredName p wm = Required "name" p (WMText wm)

data DoorConfig wm p = DoorConfig
  { name :: RequiredName p wm
  , description :: WMText wm
  , front :: Required "door front side" p (RoomEntity, WMDirection wm)
  , back :: Required "door back side" p (RoomEntity, WMDirection wm)
  , initialAppearance :: WMText wm
  , thingModify :: Eff '[State (Thing wm)] ()
  , doorModify :: Eff '[State Door] ()
  , isOpened :: Opened
  , locked :: Locked
  }

newDoor :: IsString (WMText wm) => DoorConfig wm 'Defaults
newDoor = DoorConfig
  { name = RP ()
  , description = ""
  , front = RP ()
  , back = RP ()
  , initialAppearance = ""
  , thingModify = pass
  , doorModify = pass
  , isOpened = Closed
  , locked = Unlocked
  }

addDoor ::
  forall wm es.
  AddObjects wm es
  => DoorConfig wm 'Complete
  -> Eff es DoorEntity
addDoor DoorConfig{..} = do
  let ds = blankDoor (fst front) (fst back) & (`runLocalState` doorModify)
  d <- addThing @wm name ! #initialAppearance initialAppearance ! #description description
    ! #specifics (inj (Proxy @wm) $ DoorSpecifics ds)
    ! #modify (do
      thingModify
      #objectData % #portable .= FixedInPlace
      #objectData % #pushableBetweenRooms .= False
      )
    ! #type (ObjectKind "door")
    ! #location (coerceTag $ fst front)
    ! done
      -- A door is always fixed in place.
      -- A door is never pushable between rooms.
  updateMultiLocatedObject d
  let tagged = tagEntity @Door @DoorTag ds d
  addDoorToConnection tagged front back
  pure (tagEntity ds d)