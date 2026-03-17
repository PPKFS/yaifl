{-|
Module      : Yaifl.Tag
Copyright   : (c) Avery 2023-2024
License     : MIT
Maintainer  : ppkfs@outlook.com

Type-safe entity tagging system.

This module provides the foundation for Yaifl's type-safe entity reference system.
It enables compile-time verification of entity types, eliminating runtime type errors
and reducing the need for `Maybe` wrappers in many common operations.

Key concepts:
- **Tagged entities**: Entities with phantom type tags indicating their kind
- **Type witnesses**: Values that prove an entity has a certain property
- **Safe casting**: Compile-time verified type conversions
- **Tagged objects**: Pairs of tagged entities with their corresponding objects

The tagging system solves several important problems:
1. **Type safety**: Ensures entities are used correctly based on their properties
2. **Performance**: Eliminates runtime type checks and `Maybe` unwrapping
3. **Expressiveness**: Enables writing functions that require specific entity properties
4. **Composability**: Works seamlessly with Yaifl's component system

Example usage:
@
  -- Safely tag an entity as a Thing
  thingEntity <- tagEntity thingWitness playerEntity
  
  -- Coerce between compatible tags
  enclosingEntity <- coerceTag roomEntity
  
  -- Create tagged objects
  taggedThing <- tagObject thingWitness thingEntity
@

This module is foundational for Yaifl's type system, enabling many of the
library's safety guarantees and expressive features.

Related modules:
- `Yaifl.Entity`: Basic entity definitions
- `Yaifl.ObjectLike`: Object resolution typeclasses
- `Yaifl.Enclosing.Kind`: Enclosing component definitions
-}

module Yaifl.Tag
  ( -- * Core Types
    Taggable(..)
  , TaggedObject
  , unTagObject
  
  -- * Tagging Functions
  , coerceTag
  , unsafeTagObject
  , getTaggedObject
  , getTag
  , tagObject
  
  -- * Re-exports
  , TaggedEntity
  ) where

import Yaifl.Prelude
import Yaifl.Entity

-- | Object paired with its tagged entity reference.
--
-- This newtype wraps a tuple of a `TaggedEntity` and its corresponding object.
-- It provides type-safe access to objects while maintaining their entity identity.
--
-- The `TaggedObject` type is used extensively in Yaifl for operations that
-- require both an entity reference and the object itself, such as:
-- - Object resolution
-- - Component access
-- - Spatial relationships
-- - Property manipulation
--
-- Example:
-- @
--   -- Create a tagged object from an entity and object
--   taggedThing <- tagObject thingWitness thingEntity
--   
--   -- Access the components
--   let entity = getTag taggedThing
--       object = getTaggedObject taggedThing
-- @
newtype TaggedObject o tagEntity = TaggedObject { unTagObject :: (TaggedEntity tagEntity, o) }
  deriving stock (Generic)

instance HasEntity (TaggedObject o tagEntity) where
  getEntity = getEntity . fst . unTagObject

instance Display o => Display (TaggedObject o tag) where
  displayBuilder = displayBuilder . snd . unTagObject

-- | Create a tagged object without type checking.
--
-- This function creates a `TaggedObject` directly from an object, using
-- `unsafeTagEntity` to create the tagged entity reference. It should only
-- be used when you are certain the object has the specified tag type.
--
-- Parameters:
-- - `o`: The object to tag
--
-- Returns: A tagged object with the specified tag type
--
-- Safety: This function bypasses type checking. Prefer `tagObject` when possible.
--
-- Example:
-- @
--   -- Only use when you're certain of the type
--   taggedThing <- unsafeTagObject thing
-- @
unsafeTagObject ::
  HasEntity o
  => o
  -> TaggedObject o tagEntity
unsafeTagObject o = TaggedObject (unsafeTagEntity $ getEntity o, o)

-- | Typeclass for safe entity tagging.
--
-- This typeclass defines the core operation for converting entities to tagged
-- entities given a type witness. It enables compile-time verification that an
-- entity has the required properties.
--
-- Type parameters:
-- - `taggableWith`: The witness type (e.g., `ThingWitness`, `RoomWitness`)
-- - `taggableTo`: The target tag type (e.g., `ThingTag`, `RoomTag`)
--
-- Minimal complete definition: `tagEntity` (with sensible default provided)
--
-- The default implementation uses `unsafeTagEntity`, so instances should
-- override it only when additional safety checks are needed.
--
-- Example:
-- @
--   -- Tag an entity as a Thing using a witness
--   thingEntity <- tagEntity thingWitness playerEntity
-- @
class Taggable taggableWith taggableTo where
  -- | Tag an entity with a specific type.
  --
  -- Converts an entity to a `TaggedEntity` of the specified type, using
  -- a witness to prove the conversion is valid.
  --
  -- Parameters:
  -- - `taggableWith`: Witness proving the entity has the required properties
  -- - `e`: Entity to tag
  --
  -- Returns: Tagged entity with the specified type
  --
  -- Example:
  -- @
  --   -- Tag a room entity as an enclosing
  --   enclosingEntity <- tagEntity enclosingWitness roomEntity
  -- @
  tagEntity :: HasEntity e => taggableWith -> e -> TaggedEntity taggableTo
  default tagEntity :: HasEntity e => taggableWith -> e -> TaggedEntity taggableTo
  tagEntity _ = unsafeTagEntity . getEntity

-- | Trivial instance: any entity can be tagged as itself
instance Taggable (TaggedEntity a) a

-- | Doors can be tagged as things (since doors are things with special properties)
instance Taggable DoorEntity ThingTag

-- | Tagged objects can be tagged with their own tag
instance Taggable (TaggedObject o tag) tag

-- | Extract the object from a tagged object.
--
-- Returns the object component of a `TaggedObject`, discarding the entity reference.
--
-- Parameters:
-- - `taggedObj`: The tagged object
--
-- Returns: The contained object
--
-- Example:
-- @
--   -- Get the thing from a tagged thing
--   thing <- getTaggedObject taggedThing
-- @
getTaggedObject ::
  TaggedObject o tagEntity
  -> o
getTaggedObject = snd . unTagObject

-- | Extract the entity from a tagged object.
--
-- Returns the tagged entity component of a `TaggedObject`, discarding the object.
--
-- Parameters:
-- - `taggedObj`: The tagged object
--
-- Returns: The tagged entity reference
--
-- Example:
-- @
--   -- Get the entity reference from a tagged object
--   entity <- getTag taggedThing
-- @
getTag ::
  TaggedObject o tagEntity
  -> TaggedEntity tagEntity
getTag = fst . unTagObject

-- | Create a tagged object from an entity and witness.
--
-- This is the primary function for creating `TaggedObject` values.
-- It combines an entity with a type witness to produce a tagged object
-- that can be used in type-safe operations.
--
-- Parameters:
-- - `tagWith`: Witness proving the entity has the required properties
-- - `e`: Entity to tag
--
-- Returns: A tagged object combining the entity and its type information
--
-- Example:
-- @
--   -- Create a tagged thing from an entity
--   taggedThing <- tagObject thingWitness thingEntity
--   
--   -- Use in type-safe operations
--   processTaggedThing taggedThing
-- @
--
-- This function provides the type safety that makes the tagging system valuable.
-- It should be preferred over `unsafeTagObject` whenever possible.
tagObject ::
  Taggable tagWith taggableTo
  => HasEntity e
  => tagWith
  -> e
  -> TaggedObject e taggableTo
tagObject ev e = TaggedObject (tagEntity ev e, e)

-- | Rooms can be tagged as enclosings (since all rooms are enclosings)
instance Taggable (TaggedEntity RoomTag) EnclosingTag where
  tagEntity = const . coerce

-- | People can be tagged as enclosings (since people can contain things)
instance Taggable (TaggedEntity PersonTag) EnclosingTag

-- | People can be tagged as things (since people are things with special properties)
instance Taggable (TaggedEntity PersonTag) ThingTag

-- | Coerce between compatible entity tags.
--
-- This function provides a convenient way to convert between entity tags when
-- the conversion is known to be safe. It's equivalent to calling `tagEntity`
-- with the entity itself as the witness.
--
-- Parameters:
-- - `a`: The tagged entity to coerce
--
-- Returns: The same entity with a different tag
--
-- Example:
-- @
--   -- Coerce a room entity to an enclosing entity
--   enclosingEntity <- coerceTag roomEntity
-- @
--
-- This function is particularly useful when you have a tagged entity and need
-- to use it in a context that expects a different (but compatible) tag.
coerceTag ::
  forall b a.
  Taggable (TaggedEntity a) b
  => TaggedEntity a
  -> TaggedEntity b
coerceTag a = tagEntity a (unTagEntity a)