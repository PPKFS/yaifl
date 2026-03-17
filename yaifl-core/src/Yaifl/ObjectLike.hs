{-|
Module      : Yaifl.ObjectLike
Copyright   : (c) Avery 2022-2025
License     : MIT
Maintainer  : ppkfs@outlook.com

Typeclasses for object resolution and kind checking.

This module defines the core typeclasses that enable polymorphic object access
and type-safe resolution in the Yaifl system. It provides:

- `ObjectLike`: Resolution to any game object
- `ThingLike`: Resolution to thing objects specifically
- `RoomLike`: Resolution to room objects specifically
- `objectIsKind`: Runtime kind checking for objects

These typeclasses form the foundation of Yaifl's type-safe object system,
enabling polymorphic functions that work with different object representations
(entities, tagged entities, direct objects, etc.) while maintaining type safety.

Key concepts:
- **Resolution**: Converting different object representations to concrete types
- **Polymorphism**: Writing functions that work with multiple object types
- **Type safety**: Compile-time guarantees about object types
- **Runtime checking**: Kind verification for dynamic type queries

Example usage:
@
  -- Function that works with any object-like value
  printObjectName :: ObjectLike wm o => o -> Eff es Text
  printObjectName obj = do
    anyObj <- getObject obj
    return (anyObj ^. #name)

  -- Function that requires a thing
  useThing :: ThingLike wm o => o -> Eff es ()
  useThing thing = do
    concreteThing <- getThing thing
    -- Work with the concrete thing...
@

This system enables writing generic code that works across different object
representations while providing type-safe access to specific object types.
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
-- This typeclass defines the fundamental operation for accessing game objects:
-- converting various object representations (entities, tagged entities, direct objects)
-- into the universal `AnyObject` representation.
--
-- The `getObject` method provides type-safe access to objects with proper error handling.
-- It requires the `WithoutMissingObjects` constraint to ensure the object exists,
-- and `HasCallStack` for better error messages.
--
-- Minimal complete definition: `getObject`
--
-- Instances should ensure that:
-- - The object exists (enforced by constraints)
-- - The resolution is type-safe
-- - Errors are informative
--
-- Example:
-- @
--   -- Resolve an entity to an AnyObject
--   anyObj <- getObject entity
--   -- Now work with the universal object representation
-- @
class HasEntity o => ObjectLike wm o where
  -- | Resolve an object-like value to its `AnyObject` representation.
  --
  -- This is the core method that enables polymorphic object access.
  -- It handles the conversion from various object representations to the
  -- universal `AnyObject` type.
  --
  -- Parameters:
  -- - `o`: The object-like value to resolve (entity, tagged entity, etc.)
  --
  -- Returns: The resolved `AnyObject` in the effect monad
  --
  -- Throws: `MissingObject` if the object doesn't exist (though the constraint
  --         should prevent this in most cases)
  getObject :: (HasCallStack, WithoutMissingObjects wm es) => o -> Eff es (AnyObject wm)

-- | Objects that can be resolved to `Thing`.
--
-- This typeclass extends `ObjectLike` to provide type-safe access to thing objects.
-- It's used for operations that specifically require things (portable objects).
--
-- The `getThing` method resolves object-like values directly to `Thing` types,
-- providing stronger type guarantees than `getObject`.
--
-- Minimal complete definition: `getThing`
--
-- Example:
-- @
--   -- Resolve an entity to a concrete Thing
--   thing <- getThing entity
--   -- Now work with thing-specific operations
--   pickUp thing
-- @
class HasEntity o => ThingLike wm o where
  -- | Resolve an object-like value to its `Thing` representation.
  --
  -- This method provides direct access to thing objects, bypassing the
  -- `AnyObject` intermediate representation when the type is known.
  --
  -- Parameters:
  -- - `o`: The object-like value to resolve
  --
  -- Returns: The resolved `Thing` in the effect monad
  --
  -- Throws: `MissingObject` if resolution fails
  getThing :: (HasCallStack, WithoutMissingObjects wm es) => o -> Eff es (Thing wm)

-- | Objects that can be resolved to `Room`.
--
-- This typeclass extends `ObjectLike` to provide type-safe access to room objects.
-- It's used for operations that specifically require rooms (locations).
--
-- The `getRoom` method resolves object-like values directly to `Room` types,
-- providing stronger type guarantees than `getObject`.
--
-- Minimal complete definition: `getRoom`
--
-- Example:
-- @
--   -- Resolve an entity to a concrete Room
--   room <- getRoom entity
--   -- Now work with room-specific operations
--   describeRoom room
-- @
class HasEntity o => RoomLike wm o where
  -- | Resolve an object-like value to its `Room` representation.
  --
  -- This method provides direct access to room objects, bypassing the
  -- `AnyObject` intermediate representation when the type is known.
  --
  -- Parameters:
  -- - `o`: The object-like value to resolve
  --
  -- Returns: The resolved `Room` in the effect monad
  --
  -- Throws: `MissingObject` if resolution fails
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
  getThing o = fromMaybe (error $ "tagged (thing) entity could not resolve " <> show o) . preview _Thing <$> getObject (unTagEntity o)

instance RoomLike wm (TaggedEntity RoomTag) where
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
  getObject e = if isThing (getEntity e)
    then lookupThing e >>= either (throwError . flip MissingObject e) (return . review _Thing)
    else lookupRoom e >>= either (throwError . flip MissingObject e) (return . review _Room)

instance ThingLike wm (TaggedObject (Thing wm) o) where
  getThing = pure . snd . unTagObject

instance RoomLike wm (TaggedObject (Room wm) o) where
  getRoom = pure . snd . unTagObject

-- | Check if an object is of a specific kind.
--
-- This function performs runtime kind checking on object-like values.
-- It resolves the object and checks if it matches the specified kind,
-- supporting the full kind hierarchy defined in `Yaifl.KindGraph`.
--
-- This is useful for dynamic type queries where compile-time type information
-- is insufficient, such as when working with heterogeneous collections or
-- processing player input.
--
-- Example:
-- @
--   -- Check if an object is a container
--   isContainer <- objectIsKind "container" someObject
--   if isContainer
--     then openContainer someObject
--     else report "That's not a container."
--
--   -- Check kind hierarchy
--   isThing <- objectIsKind "thing" someObject  -- True for things, containers, etc.
-- @
--
-- Parameters:
-- - `t`: The `ObjectKind` to check against (e.g., "thing", "container", "person")
-- - `o`: The object-like value to check
--
-- Returns: `True` if the object is of the specified kind or inherits from it,
--          `False` otherwise
--
-- Note: This function uses the kind hierarchy, so it will return `True` for
-- any object that inherits from the specified kind (e.g., a "container" is
-- also a "thing").
objectIsKind ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => ObjectKind  -- ^ Kind to check (e.g., "thing", "container", "person")
  -> o            -- ^ Object-like value to check
  -> Eff es Bool  -- ^ `True` if object matches the kind
objectIsKind t o = getObject o >>= (`isKind` t)