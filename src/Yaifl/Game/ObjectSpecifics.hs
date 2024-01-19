

module Yaifl.Game.ObjectSpecifics
  ( -- * Specifics
  ObjectSpecifics(..)
  , WMHasObjSpecifics(..)
  , addDoor
  ) where

import Solitude

import Yaifl.Model.Metadata ( noteError )
import Yaifl.Model.Kinds.Direction (WMStdDirections)
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Object
import Yaifl.Game.Create.Object
import Yaifl.Model.Effects
import Yaifl.Game.Move
import Yaifl.Model.ObjectLike
import Yaifl.Game.Create.RoomConnection
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Door
import Yaifl.Model.Kinds.Enclosing ( Enclosing )
import Yaifl.Model.HasProperty ( MayHaveProperty(..), WMWithProperty )
import Yaifl.Model.MultiLocated
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Query
import Yaifl.Model.WorldModel ( WMObjSpecifics, WorldModel(..), WMSayable, WMDirection )
import qualified Data.Set as S
import Yaifl.Model.Tag

data ObjectSpecifics =
  NoSpecifics
  | EnclosingSpecifics Enclosing
  | ContainerSpecifics Container
  | OpenabilitySpecifics Openability
  | DoorSpecifics Door
  deriving stock (Eq, Show, Read)

makePrisms ''ObjectSpecifics

instance Pointed ObjectSpecifics where
  identityElement = NoSpecifics

class WMHasObjSpecifics (wm :: WorldModel) where
  inj :: Proxy wm -> ObjectSpecifics -> WMObjSpecifics wm

instance WMHasObjSpecifics ('WorldModel ObjectSpecifics a b c ac r se) where
  inj _ = id

instance MayHaveProperty ObjectSpecifics Enclosing where
  propertyAT = _EnclosingSpecifics `thenATraverse` (_ContainerSpecifics % containerEnclosing)

instance MayHaveProperty ObjectSpecifics MultiLocated where
  propertyAT = _DoorSpecifics % #multiLocated --`thenATraverse` (_ContainerSpecifics % containerEnclosing)

instance MayHaveProperty ObjectSpecifics Container where
  propertyAT = castOptic _ContainerSpecifics

instance MayHaveProperty ObjectSpecifics Enterable where
  propertyAT = _ContainerSpecifics % containerEnterable

instance MayHaveProperty ObjectSpecifics Openability where
  propertyAT = _OpenabilitySpecifics
    `thenATraverse` (_ContainerSpecifics % containerOpenable)
    `thenATraverse` (_DoorSpecifics % #opened)

instance MayHaveProperty ObjectSpecifics Door where
  propertyAT = castOptic _DoorSpecifics

localST ::
  State st :> es
  => (st -> st)
  -> Eff es a
  -> Eff es a
localST f l = do
  b <- get
  modify f
  r <- l
  put b
  pure r

--TODO: I am only going to enforce implications as a quickcheck or hedgehog
-- invariant, namely that the door type will have a smart ctr (addDoor) that makes sure
-- it isn't portable on creation, and then the check will be whenever we modify an object
-- make sure that it isn't breaking an invariant.
addDoor ::
  forall wm es.
  WMHasObjSpecifics wm
  => WMWithProperty wm Enclosing
  => WMStdDirections wm
  => WMWithProperty wm MultiLocated
  => NoMissingObjects wm es
  => AddObjects wm es
  => WMSayable wm -- ^ name
  -> WMSayable wm -- ^ initial appearance
  -> WMSayable wm -- ^ description
  -> (RoomEntity, WMDirection wm)
  -> (RoomEntity, WMDirection wm)
  -> Maybe (ThingData wm) -- ^ Optional details; if 'Nothing' then the default is used.
  -> Eff es DoorEntity
addDoor n ia des f b mbD = do
  let ds = blankDoor (fst f) (fst b)
  d <- addThingInternal n ia des (ObjectType "door")
      (Just $ inj (Proxy @wm) $ DoorSpecifics ds)
      -- A door is always fixed in place.
      -- A door is never pushable between rooms.
      (Just $ (\x -> x & #portable .~ FixedInPlace & #pushableBetweenRooms .~ False) $ fromMaybe (blankThingData ia) mbD)
      (Just (coerceTag $ fst f))
  updateMultiLocatedObject d
  let tagged = tag @Door @DoorTag ds d
  addDoorToConnection tagged f b
  pure (tag ds d)

updateMultiLocatedObject ::
  WMWithProperty wm MultiLocated
  => WMWithProperty wm Enclosing
  => NoMissingObjects wm es
  => ThingLike wm tl
  => tl
  -> Eff es ()
updateMultiLocatedObject tl = do
  t <- getThing tl
  case getMultiLocatedMaybe t of
    Nothing -> noteError (const ()) "the object had no multilocated component"
    Just ml -> mapM_ (\x -> do
      obj <- getObject x
      let enc = getEnclosing x obj
      updateToContain obj enc t) (S.toList $ ml ^. #locations)
