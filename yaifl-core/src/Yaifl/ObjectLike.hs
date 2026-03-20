{-|
Module      : Yaifl.ObjectLike
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Typeclasses for object resolution and kind checking to enable polymorphic object access.

It provides:

- `ObjectLike`: Resolution to any game object
- `ThingLike`: Resolution to Thing objects specifically
- `RoomLike`: Resolution to Room objects specifically

See also:
- `Yaifl.AnyObject` for the universal object representation
-}

module Yaifl.ObjectLike
  ( ObjectLike(..)
  , ThingLike(..)
  , RoomLike(..)
  , objectIsKind
  ) where

import Yaifl.Prelude

import Effectful.Error.Static

import Yaifl.AnyObject
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Metadata ( isKind )
import Yaifl.Object.Kind
import Yaifl.Room.Kind
import Yaifl.Tag
import Yaifl.Thing.Kind

-- | Objects that can be resolved to `AnyObject`.
--
-- Fundamental operation for accessing game objects by converting various
-- representations to `AnyObject`.
class HasEntity o => ObjectLike wm o where
  -- | Resolve an object-like value to its `AnyObject` representation for polymorphic object access.
  --
  -- Throws: `MissingObject` if the object doesn't exist.
  getObject :: (HasCallStack, WithoutMissingObjects wm es) => o -> Eff es (AnyObject wm)

-- | Objects that can be resolved to `Thing`.
class HasEntity o => ThingLike wm o where
  -- | Resolve an object-like value to its `Thing` representation.
  getThing :: (HasCallStack, WithoutMissingObjects wm es) => o -> Eff es (Thing wm)

-- | Objects that can be resolved to `Room`.
class HasEntity o => RoomLike wm o where
  -- | Resolve an object-like value to its `Room` representation.
  getRoom :: (HasCallStack, WithoutMissingObjects wm es) => o -> Eff es (Room wm)

instance (ObjectLike wm o) => ObjectLike wm (TaggedObject o tagEntity) where
  getObject = getObject . snd . unTagObject

instance {-# OVERLAPPABLE #-} (RoomLike wm o) => RoomLike wm (TaggedObject o tagEntity) where
  getRoom = getRoom . snd . unTagObject

instance {-# OVERLAPPABLE #-} (ThingLike wm o) => ThingLike wm (TaggedObject o tagEntity) where
  getThing = getThing . snd . unTagObject

instance ObjectLike wm (Thing wm) where
  getObject = pure . toAny

instance ObjectLike wm (Room wm) where
  getObject = pure . toAny

instance ThingLike wm (Thing wm) where
  getThing = pure

instance RoomLike wm (Room wm) where
  getRoom = pure

instance ThingLike wm (TaggedEntity ThingTag) where
  -- | Resolve a tagged thing entity by looking up the entity and extracting the thing.
  -- Uses error-throwing fallback if resolution fails (shouldn't happen with valid entities).
  getThing o = fromMaybe (error $ "tagged (thing) entity could not resolve " <> show o) . preview _Thing <$> getObject (unTagEntity o)

instance RoomLike wm (TaggedEntity RoomTag) where
  -- | Resolve a tagged room entity by looking up the entity and extracting the room.
  -- Uses error-throwing fallback if resolution fails (shouldn't happen with valid entities).
  getRoom o = fromMaybe (error $ "tagged (room) entity could not resolve " <> show o) . preview _Room <$> getObject (unTagEntity o)

instance ObjectLike wm (AnyObject wm) where
  getObject = pure

instance ObjectLike wm (TaggedEntity anyTag) where
  getObject e = getObject (unTagEntity e)

instance ThingLike wm DoorEntity where
  getThing = getThing . coerceTag @ThingTag

instance ThingLike wm PersonEntity where
  getThing = getThing . coerceTag @ThingTag

instance ObjectLike wm Entity where
  -- | Resolve an entity to `AnyObject` by checking its type and performing appropriate lookup.
  -- Uses proper error handling via `MissingObject` for invalid entities.
  getObject e = if isThing (getEntity e)
    then lookupThing e >>= either (throwError . flip MissingObject e) (return . review _Thing)
    else lookupRoom e >>= either (throwError . flip MissingObject e) (return . review _Room)

instance ThingLike wm (TaggedObject (Thing wm) o) where
  getThing = pure . snd . unTagObject

instance RoomLike wm (TaggedObject (Room wm) o) where
  getRoom = pure . snd . unTagObject

-- | Check if an object is of a specific kind.
--
-- Runtime kind checking that resolves the object and checks against the kind hierarchy.
-- Does not guarantee properties, only kind membership.
objectIsKind ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => ObjectKind  -- ^ Kind to check (e.g., "thing", "container", "person")
  -> o            -- ^ Object-like value to check
  -> Eff es Bool  -- ^ `True` if object matches the kind
objectIsKind t o = getObject o >>= (`isKind` t)