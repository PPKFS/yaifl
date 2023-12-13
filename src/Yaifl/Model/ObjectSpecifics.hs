{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Model.ObjectSpecifics
  ( -- * Specifics
  ObjectSpecifics(..)
  , WMHasObjSpecifics(..)
  , addDoor
  ) where

import Solitude

import Yaifl.Metadata ( ObjectType(..) )
import Yaifl.Model.Object
import Yaifl.Model.Objects.Create
import Yaifl.Model.Objects.ThingData
import Yaifl.Model.Properties.Enclosing ( Enclosing )
import Yaifl.Model.Properties.Has ( MayHaveProperty(..), WMWithProperty )
import Yaifl.Model.WorldModel ( WMObjSpecifics, WorldModel(..), WMSayable, WMDirection )
import Yaifl.Model.Properties.Container
import Yaifl.Model.Properties.Door
import Yaifl.Model.Properties.Openable ( Openable )
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Entity
import Yaifl.Model.Objects.RoomConnections
import Yaifl.Model.Direction (WMStdDirections)

data ObjectSpecifics =
  NoSpecifics
  | EnclosingSpecifics Enclosing
  | ContainerSpecifics Container
  | OpenableSpecifics Openable
  | DoorSpecifics DoorSpecifics
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

instance MayHaveProperty ObjectSpecifics Container where
  propertyAT = castOptic _ContainerSpecifics

instance MayHaveProperty ObjectSpecifics Enterable where
  propertyAT = _ContainerSpecifics % containerEnterable

instance MayHaveProperty ObjectSpecifics Openable where
  propertyAT = _OpenableSpecifics `thenATraverse` (_ContainerSpecifics % containerOpenable)

instance MayHaveProperty ObjectSpecifics DoorSpecifics where
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
  => NoMissingObjects wm es
  => AddObjects wm es
  => WMSayable wm -- ^ name
  -> Maybe (WMSayable wm) -- ^ description
  -> (Room wm, WMDirection wm)
  -> (Room wm, WMDirection wm)
  -> Maybe (ThingData wm) -- ^ Optional details; if 'Nothing' then the default is used.
  -> Eff es (Thing wm)
addDoor n mbDes f b mbD = do
  let ds = blankDoorSpecifics (tagRoom (fst f)) (tagRoom (fst b))
  d <- addThingInternal n (fromMaybe "" mbDes) (ObjectType "door")
      (Just $ inj (Proxy @wm) $ DoorSpecifics ds)
      (Just $ (\x -> x & #portable .~ FixedInPlace & #pushableBetweenRooms .~ False) $ fromMaybe blankThingData mbD)
  addDoorToConnection (tag @DoorSpecifics @DoorTag ds d) f b
  pure d