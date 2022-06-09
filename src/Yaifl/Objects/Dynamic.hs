-- ~\~ language=Haskell filename=src/Yaifl/Objects/Dynamic.hs
-- ~\~ begin <<lit/worldmodel/objects/dynamic.md|src/Yaifl/Objects/Dynamic.hs>>[0] project://lit/worldmodel/objects/dynamic.md:8

{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Objects.Dynamic 
  ( -- * Types
    TimestampedObject(..)
  , ObjectUpdate(..)
  , AbstractObject(..)
  , AbstractThing
  , AbstractRoom
  , AnyAbstractObject
  -- * Lenses
  , tsCachedObject
  , tsCacheStamp
  , tsUpdateFunc
  ) where

import Solitude ( Generic, Either, makeLenses, Eff )
import Yaifl.Common ( WorldModel, Timestamp, HasID(..) )
import Yaifl.Objects.Object ( Object )
import Yaifl.Objects.ObjectData ( RoomData, ThingData )

-- ~\~ begin <<lit/worldmodel/objects/dynamic.md|timestamped-object>>[0] project://lit/worldmodel/objects/dynamic.md:37
data TimestampedObject wm d = TimestampedObject
  { _tsCachedObject :: !(Object wm d)
  , _tsCacheStamp :: !Timestamp
  , _tsUpdateFunc :: ObjectUpdate wm d
  } deriving stock (Generic)

instance HasID (TimestampedObject wm d) where
  getID (TimestampedObject o _ _) = getID o

-- | Function to update an object. It is read-only on the world; i.e. it can only modify itself
newtype ObjectUpdate (wm :: WorldModel) d = ObjectUpdate
  { runObjectUpdate :: forall es. Object wm d -> Eff es (Object wm d)
  } 
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/objects/dynamic.md|abstract-object>>[0] project://lit/worldmodel/objects/dynamic.md:54
data AbstractObject wm d
  = DynamicObject (TimestampedObject wm d)
  | StaticObject (Object wm d)

instance HasID (AbstractObject wm d) where
  getID (StaticObject o) = getID o
  getID (DynamicObject ts) = getID ts

type AbstractThing wm = AbstractObject wm ThingData
type AbstractRoom wm = AbstractObject wm (RoomData wm)
type AnyAbstractObject wm = AbstractObject wm (Either ThingData (RoomData wm))

makeLenses ''TimestampedObject
-- ~\~ end
-- ~\~ end
