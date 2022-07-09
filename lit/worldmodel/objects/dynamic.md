# Dynamic Objects

One of the coolest features of Inform is to have attributes and properties of objects be dynamic; for instance, `Slightly Wrong` shows a room with a dynamic description. Originally I had this only for descriptions but it made more sense to be applied to entire objects.

The only times that we actually care about the underlying representation of an object is when creating them (because we need to supply an update function) and when reifying `AbstractObject`s from a `State`-based implementation. This is a big refactor I'm happy to have made because it cleanly breaks apart implementation (cached objects) and semantics (objects at a point in time). It does have the slight issue that we need to be cautious: if we reify a dynamic object at some point in time, then we change some part of the world that may affect its update function, we will need to re-reify the object.

```haskell file=src/Yaifl/Core/Objects/Dynamic.hs

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

import Solitude ( Generic, Either, makeLenses, Eff )
import Yaifl.Core.Common ( WorldModel, Timestamp, HasID(..) )
import Yaifl.Core.Objects.Object ( Object )
import Yaifl.Core.Objects.ObjectData ( RoomData, ThingData )

<<timestamped-object>>
<<abstract-object>>
```

## Timestamped Objects

```haskell id=timestamped-object
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
```

## Abstract Objects
```haskell id=abstract-object
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
```
