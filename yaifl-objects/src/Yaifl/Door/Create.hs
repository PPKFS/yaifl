{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Yaifl.Door.Create
  ( addDoor
  , newDoor
  , DoorConfig(..)
  , Purpose(..)
  , RequiredParameter(..)
  , Required
  , RequiredName

  , isToThe
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
import Yaifl.Openable.Kind
import Yaifl.Builder
import Yaifl.Direction.Kind

--TODO: I am only going to enforce implications as a quickcheck or hedgehog
-- invariant, namely that the door type will have a smart ctr (addDoor) that makes sure
-- it isn't portable on creation, and then the check will be whenever we modify an object
-- make sure that it isn't breaking an invariant.

data DoorLockStatus = NotLockable | Lockable Lockability

lockStatusFromMaybe :: Maybe Lockability -> DoorLockStatus
lockStatusFromMaybe = maybe NotLockable Lockable
data DoorConfig wm p = DoorConfig
  { description :: WMText wm
  , front :: (RoomEntity, WMDirection wm)
  , back :: (RoomEntity, WMDirection wm)
  , initialAppearance :: WMText wm
  , thingModify :: Eff '[State (Thing wm)] ()
  , doorModify :: Eff '[State Door] ()
  , openStatus :: (Opened, Openable)
  , lockStatus :: DoorLockStatus
  } deriving stock (Generic)

newDoor :: IsString (WMText wm) => (RoomEntity, WMDirection wm) -> (RoomEntity, WMDirection wm) -> DoorConfig wm 'Complete
newDoor front back = DoorConfig
  { description = ""
  , initialAppearance = ""
  , thingModify = pass
  , doorModify = pass
  , lockStatus = lockStatusFromMaybe $ defaultDoorOpenability ^. #lockability
  , openStatus = let Openability {opened, openable} = defaultDoorOpenability in (opened, openable)
  , front
  , back
  }

isToThe :: RoomEntity -> Direction -> (RoomEntity, Direction)
isToThe r d = (r, d)

addDoor ::
  forall wm es.
  AddObjects wm es
  => WMText wm
  -> DoorConfig wm 'Complete
  -> Eff es DoorEntity
addDoor name DoorConfig{..} = do
  let ds = blankDoor (fst front) (fst back) & (`runLocalState` doorModify)
  d <- addThing name newThing
        { initialAppearance
        , description
        , specifics = inj (Proxy @wm) $ DoorSpecifics ds
        , thingModify = do
            thingModify
            #objectData % #portable .= FixedInPlace
            #objectData % #pushableBetweenRooms .= False
        , objType = ObjectKind "door"
        , location = Just $ coerceTag $ fst front
        }
      -- A door is always fixed in place.
      -- A door is never pushable between rooms.
  updateMultiLocatedObject d
  let tagged = tagEntity @Door @DoorTag ds d
  addDoorToConnection tagged front back
  pure (tagEntity ds d)