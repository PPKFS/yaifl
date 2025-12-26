module Yaifl.Door.Create
  ( addDoor
  ) where

import Yaifl.Prelude

import Yaifl.ObjectSpecifics
import Yaifl.Direction.Kind (WMStdDirections)
import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.Door.Kind
import Yaifl.Enclosing.Kind ( Enclosing (..) )
import Yaifl.Property.Has ( WMWithProperty )
import Yaifl.MultiLocated.Kind
import Yaifl.Tag
import Yaifl.WorldModel
import Yaifl.Effects.RuleEffects
import Yaifl.Thing.Create
import Yaifl.MultiLocated.Query
import Yaifl.Room.Query

--TODO: I am only going to enforce implications as a quickcheck or hedgehog
-- invariant, namely that the door type will have a smart ctr (addDoor) that makes sure
-- it isn't portable on creation, and then the check will be whenever we modify an object
-- make sure that it isn't breaking an invariant.
addDoor ::
  forall wm es.
  AddObjects wm es
  => WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => WMStdDirections wm
  => WMWithProperty wm MultiLocated
  => RuleEffects wm es
  => WMText wm -- ^ name
  -> "front" :! (RoomEntity, WMDirection wm)
  -> "back" :! (RoomEntity, WMDirection wm)
  -> "initialAppearance" :? WMText wm
  -> "description" :? WMText wm -- ^ Description.
  -> "described" :? ThingDescribed
  -> "modify" :? Eff '[State (Thing wm)] () -- ^ Build your own thing monad!
  -> "thingData" :? ThingData wm -- ^ Optional details; if 'Nothing' then the default is used.
  -> Eff es DoorEntity
addDoor n (arg #front -> f) (arg #back -> b) ia des (argDef #described Described -> desc) (argDef #modify pass -> upD) (argF #thingData -> mbD) = do
  let ds = blankDoor (fst f) (fst b)
  d <- addThing @wm n ia des
    ! #specifics (inj (Proxy @wm) $ DoorSpecifics ds)
    ! #modify (do
      upD
      #objectData % #portable .= FixedInPlace
      #objectData % #pushableBetweenRooms .= False
      #objectData % #described .= desc)
    ! #type (ObjectKind "door")
    ! paramF #thingData mbD
    ! #location (coerceTag $ fst f)
    ! done
      -- A door is always fixed in place.
      -- A door is never pushable between rooms.
  updateMultiLocatedObject d
  let tagged = tagEntity @Door @DoorTag ds d
  addDoorToConnection tagged f b
  pure (tagEntity ds d)