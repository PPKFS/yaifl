{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.ObjectSpecifics
  ( -- * Specifics
  ObjectSpecifics(..)
  , WMHasObjSpecifics(..)
  ) where

import Solitude

import Yaifl.Core.Entity ( HasID(getID) )
import Yaifl.Core.Metadata (previousRoom, ObjectType(..))
import Yaifl.Core.Object
import Yaifl.Core.Objects.Create ( AddObjects, addThing )
import Yaifl.Core.Objects.Dynamic ( ObjectUpdateFunc )
import Yaifl.Core.Objects.ThingData ( ThingData )
import Yaifl.Core.Properties.Enclosing ( Enclosing )
import Yaifl.Core.Properties.Has ( HasProperty(..), WMHasProperty )
import Yaifl.Core.WorldModel ( WMObjSpecifics, WorldModel(..) )
import Yaifl.Lamp.Properties.Container
import Yaifl.Lamp.Properties.Door ( Door, blankDoor )
import Yaifl.Lamp.Properties.Openable ( Openable )

data ObjectSpecifics =
  NoSpecifics
  | EnclosingSpecifics Enclosing
  | ContainerSpecifics Container
  | OpenableSpecifics Openable
  | DoorSpecifics Door
  deriving stock (Eq, Show, Read)

makePrisms ''ObjectSpecifics

class WMHasObjSpecifics (wm :: WorldModel) where
  inj :: Proxy wm -> ObjectSpecifics -> WMObjSpecifics wm

instance WMHasObjSpecifics ('WorldModel ObjectSpecifics a b c) where
  inj _ = id

instance HasProperty ObjectSpecifics Enclosing where
  propertyL = _EnclosingSpecifics `thenATraverse` (_ContainerSpecifics % containerEnclosing)

instance HasProperty ObjectSpecifics Container where
  propertyL = castOptic _ContainerSpecifics

instance HasProperty ObjectSpecifics Enterable where
  propertyL = _ContainerSpecifics % containerEnterable

instance HasProperty ObjectSpecifics Openable where
  propertyL = _OpenableSpecifics `thenATraverse` (_ContainerSpecifics % containerOpenable)

instance HasProperty ObjectSpecifics Door where
  propertyL = castOptic _DoorSpecifics

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
  forall wm es. WMHasObjSpecifics wm
  => WMHasProperty wm Enclosing
  => AddObjects wm es
  => SayableText -- ^ name
  -> Maybe SayableText -- ^ description
  -> Room wm
  -> Room wm
  -> Maybe ThingData -- ^ Optional details; if 'Nothing' then the default is used.
  -> Maybe (ObjectUpdateFunc wm ThingData) -- ^ maybe some extra stuff
  -> Eff es (Thing wm)
addDoor n mbDes fr ba mbD mbUpd = localST (#previousRoom .~ objectId fr) $ do
    addThing n (fromMaybe "" mbDes) (ObjectType "door")
      (Just (inj (Proxy @wm) (DoorSpecifics (blankDoor (getID ba))))) mbD mbUpd