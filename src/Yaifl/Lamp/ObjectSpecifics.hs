-- ~\~ language=Haskell filename=src/Yaifl/Lamp/ObjectSpecifics.hs
-- ~\~ begin <<lit/worldmodel/objects/specifics.md|src/Yaifl/Lamp/ObjectSpecifics.hs>>[0] project://lit/worldmodel/objects/specifics.md:6
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.ObjectSpecifics
  ( -- * Specifics
  ObjectSpecifics(..)
  , WMHasObjSpecifics(..)
  ) where


import Yaifl.Lamp.Properties.Container
import Yaifl.Core.Properties.Enclosing
import Yaifl.Lamp.Properties.Openable
import Yaifl.Core.Properties.Property ( HasProperty(..), WMHasProperty )
import Yaifl.Lamp.Properties.Door
import Yaifl.Core.Objects.Object
import Yaifl.Core.Objects.Dynamic
import Yaifl.Core.Objects.ObjectData
import Cleff.State
import Yaifl.Core.Common
import Yaifl.Core.Objects.Create

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
  => Text -- ^ name
  -> Maybe Text -- ^ description
  -> Room wm
  -> Room wm
  -> Maybe ThingData -- ^ Optional details; if 'Nothing' then the default is used.
  -> Maybe (ObjectUpdateFunc wm ThingData) -- ^ maybe some extra stuff
  -> Eff es (Thing wm)
addDoor n mbDes fr ba mbD mbUpd = localST (previousRoom .~ _objID fr) $ do
    addThing n (fromMaybe "" mbDes) (ObjType "door") 
      (Just (inj (Proxy @wm) (DoorSpecifics (blankDoor (getID ba))))) mbD mbUpd 
-- ~\~ endccV
