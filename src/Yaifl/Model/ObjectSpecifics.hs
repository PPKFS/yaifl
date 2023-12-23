

module Yaifl.Model.ObjectSpecifics
  ( -- * Specifics
  ObjectSpecifics(..)
  , WMHasObjSpecifics(..)
  , addDoor
  ) where

import Solitude

import Yaifl.Metadata ( ObjectType(..), noteError )
import Yaifl.Model.Direction (WMStdDirections)
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Object
import Yaifl.Model.Objects.Create
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.Move
import Yaifl.Model.Objects.ObjectLike
import Yaifl.Model.Objects.RoomConnections
import Yaifl.Model.Objects.ThingData
import Yaifl.Model.Properties.Container
import Yaifl.Model.Properties.Door
import Yaifl.Model.Properties.Enclosing ( Enclosing )
import Yaifl.Model.Properties.Has ( MayHaveProperty(..), WMWithProperty )
import Yaifl.Model.Properties.MultiLocated
import Yaifl.Model.Properties.Openable ( Openable )
import Yaifl.Model.Properties.Query
import Yaifl.Model.WorldModel ( WMObjSpecifics, WorldModel(..), WMSayable, WMDirection )
import qualified Data.Set as S
import Yaifl.Model.Objects.Tag

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

instance MayHaveProperty ObjectSpecifics MultiLocated where
  propertyAT = _DoorSpecifics % #multiLocated --`thenATraverse` (_ContainerSpecifics % containerEnclosing)

instance MayHaveProperty ObjectSpecifics Container where
  propertyAT = castOptic _ContainerSpecifics

instance MayHaveProperty ObjectSpecifics Enterable where
  propertyAT = _ContainerSpecifics % containerEnterable

instance MayHaveProperty ObjectSpecifics Openable where
  propertyAT = _OpenableSpecifics
    `thenATraverse` (_ContainerSpecifics % containerOpenable)
    `thenATraverse` (_DoorSpecifics % #openable)

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
  => WMWithProperty wm MultiLocated
  => NoMissingObjects wm es
  => AddObjects wm es
  => WMSayable wm -- ^ name
  -> WMSayable wm -- ^ initial appearance
  -> WMSayable wm -- ^ description
  -> (Room wm, WMDirection wm)
  -> (Room wm, WMDirection wm)
  -> Maybe (ThingData wm) -- ^ Optional details; if 'Nothing' then the default is used.
  -> Eff es (TaggedObject (Thing wm) DoorTag)
addDoor n ia des f b mbD = do
  let ds = blankDoorSpecifics (tagRoom (fst f)) (tagRoom (fst b))
  d <- addThingInternal n ia des (ObjectType "door")
      (Just $ inj (Proxy @wm) $ DoorSpecifics ds)
      (Just $ (\x -> x & #portable .~ FixedInPlace & #pushableBetweenRooms .~ False) $ fromMaybe (blankThingData ia) mbD)
      (Just (coerceTag $ tagRoom $ fst f))
  updateMultiLocatedObject d
  let tagged = tag @DoorSpecifics @DoorTag ds d
  addDoorToConnection tagged f b
  pure (tagDoorObject ds d)

updateMultiLocatedObject ::
  WMWithProperty wm MultiLocated
  => WMWithProperty wm Enclosing
  => NoMissingObjects wm es
  => Thing wm -> Eff es ()
updateMultiLocatedObject t = do
  case getMultiLocatedMaybe t of
    Nothing -> noteError (const ()) "the object had no multilocated component"
    Just ml -> mapM_ (\x -> do
      obj <- getObject x
      let enc = getEnclosing x obj
      updateToContain obj enc t) (S.toList $ ml ^. #locations)

