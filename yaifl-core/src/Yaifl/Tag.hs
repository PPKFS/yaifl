{-|
Module      : Yaifl.Tag
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Type-safe entity tagging system with witness-based verification.

Enables compile-time verification that entities have required properties,
allowing safe avoidance of Maybe in queries when type constraints are satisfied.
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
-- Newtype wrapping a `TaggedEntity` and its corresponding object.
-- Provides type-safe access to objects while maintaining entity identity.
newtype TaggedObject o tagEntity = TaggedObject { unTagObject :: (TaggedEntity tagEntity, o) }
  deriving stock (Generic)

instance HasEntity (TaggedObject o tagEntity) where
  getEntity = getEntity . fst . unTagObject

instance Display o => Display (TaggedObject o tag) where
  displayBuilder = displayBuilder . snd . unTagObject

-- | Create a tagged object without type checking.
--
-- Creates a `TaggedObject` directly from an object using `unsafeTagEntity`.
-- Should only be used when certain the object has the specified tag type.
--
-- Safety: Bypasses type checking. Prefer `tagObject` when possible.
unsafeTagObject ::
  HasEntity o
  => o
  -> TaggedObject o tagEntity
unsafeTagObject o = TaggedObject (unsafeTagEntity $ getEntity o, o)

-- | Typeclass for safe entity tagging.
--
-- Defines conversion of entities to tagged entities given a type witness.
-- Enables compile-time verification that entities have required properties.
--
-- Default implementation uses `unsafeTagEntity`. Instances should override
-- only when additional safety checks are needed.
class Taggable taggableWith taggableTo where
  -- | Tag an entity with a specific type.
  --
  -- Converts an entity to a `TaggedEntity` of the specified type using
  -- a witness to prove the conversion is valid.
  tagEntity :: HasEntity e => taggableWith -> e -> TaggedEntity taggableTo
  default tagEntity :: HasEntity e => taggableWith -> e -> TaggedEntity taggableTo
  tagEntity _ = unsafeTagEntity . getEntity

-- | Trivial instance: any entity can be tagged as itself
instance Taggable (TaggedEntity a) a

-- | Doors can be tagged as things (since doors are things with special properties)
instance Taggable DoorEntity ThingTag

-- | Tagged objects can be tagged with their own tag
instance Taggable (TaggedObject o tag) tag

-- | Rooms can be tagged as enclosings (since all rooms are enclosings)
instance Taggable (TaggedEntity RoomTag) EnclosingTag where
  tagEntity = const . coerce

-- | People can be tagged as enclosings (since people can contain things)
instance Taggable (TaggedEntity PersonTag) EnclosingTag

-- | People can be tagged as things (since people are things with special properties)
instance Taggable (TaggedEntity PersonTag) ThingTag

-- | Extract the object from a tagged object.
--
-- Returns the object component of a `TaggedObject`, discarding the entity reference.
getTaggedObject ::
  TaggedObject o tagEntity
  -> o
getTaggedObject = snd . unTagObject

-- | Extract the entity from a tagged object.
--
-- Returns the tagged entity component of a `TaggedObject`, discarding the object.
getTag ::
  TaggedObject o tagEntity
  -> TaggedEntity tagEntity
getTag = fst . unTagObject

-- | Create a tagged object from an entity and witness.
--
-- Primary function for creating `TaggedObject` values.
-- Combines an entity with a type witness to produce a tagged object
-- for type-safe operations.
--
-- Provides the type safety that makes the tagging system valuable.
-- Should be preferred over `unsafeTagObject` whenever possible.
tagObject ::
  Taggable tagWith taggableTo
  => HasEntity e
  => tagWith
  -> e
  -> TaggedObject e taggableTo
tagObject ev e = TaggedObject (tagEntity ev e, e)

-- | Coerce between compatible entity tags.
--
-- Convenient way to convert between entity tags when the conversion is safe.
-- Equivalent to calling `tagEntity` with the entity itself as the witness.
--
-- Particularly useful when you have a tagged entity and need to use it in a context
-- that expects a different (but compatible) tag, ala 'downcasting'.
coerceTag ::
  forall b a.
  Taggable (TaggedEntity a) b
  => TaggedEntity a
  -> TaggedEntity b
coerceTag a = tagEntity a (unTagEntity a)