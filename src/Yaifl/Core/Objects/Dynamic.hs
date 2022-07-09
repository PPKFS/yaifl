-- ~\~ language=Haskell filename=src/Yaifl/Core/Objects/Dynamic.hs
-- ~\~ begin <<lit/worldmodel/objects/dynamic.md|src/Yaifl/Core/Objects/Dynamic.hs>>[0] project://lit/worldmodel/objects/dynamic.md:8

{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Objects.Dynamic 
  ( -- * Types
    TimestampedObject(..)
  , ObjectUpdateFunc(..)
  , AbstractObject(..)
  , AbstractThing
  , AbstractRoom
  , AnyAbstractObject
  -- * Lenses
  , tsCachedObject
  , tsCacheStamp
  , tsUpdateFunc
  ) where

import Yaifl.Core.Common ( WorldModel, Timestamp, HasID(..) )
import Yaifl.Core.Objects.Object ( Object )
import Yaifl.Core.Objects.ObjectData ( RoomData, ThingData )

-- ~\~ begin <<lit/worldmodel/objects/dynamic.md|timestamped-object>>[0] project://lit/worldmodel/objects/dynamic.md:37
data TimestampedObject wm d = TimestampedObject
  { _tsCachedObject :: !(Object wm d)
  , _tsCacheStamp :: !Timestamp
  , _tsUpdateFunc :: ObjectUpdateFunc wm d
  } deriving stock (Generic)

instance HasID (TimestampedObject wm d) where
  getID (TimestampedObject o _ _) = getID o

-- | Function to update an object. It is read-only on the world; i.e. it can only modify itself
newtype ObjectUpdateFunc (wm :: WorldModel) d = ObjectUpdateFunc
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
