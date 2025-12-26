{-|
Module      : Yaifl.Entity
Copyright   : (c) Avery 2022-2025
License     : MIT
Maintainer  : ppkfs@outlook.com

An `Entity` is an object ID. They are used to uniquely identify game objects. Entities should only be generated using `Yaifl.Core.ObjectQuery.generateEntity`,
which ensures IDs are globally unique. Positive IDs should be used for `Yaifl.Thing.Kind.Thing`s and negative IDs should be used for `Yaifl.Room.Kind.Room`s.

`Entity` keys can also be used for general `Yaifl.Store`s.
-}

module Yaifl.Entity
  ( -- * Entities
    Entity(..)
  , HasID(..)
  -- ** TaggedEntity
  -- | Entities tagged with a phantom type that allows for additional type-safety when making indirect references.
  , TaggedEntity(unTagEntity)
  , unsafeTagEntity
  -- ** Tags
  -- | These phantom types are defined here because these ones are foundational enough that they need to be
  -- forward-declared. That is, we need to be able to refer to type-safe `Entity`s at this level of the library
  -- that are also:
  -- `Yaifl.Room.Kind`s and `Yaifl.Thing.Kind`s (as these are two of the key types that everything else is based around);
  -- `Yaifl.Core.Kinds.Door`s and `Yaifl.Enclosing.Kind` (as these are part of the definition of `Yaifl.Room.Kind`)
  -- `Yaifl.Core.Kinds.Person` to store a reference to the current player in `Yaifl.Metadata`.
  , ThingTag
  , ThingEntity
  , RoomTag
  , RoomEntity
  , EnclosingTag
  , EnclosingEntity
  , DoorTag
  , DoorEntity
  , PersonTag
  , PersonEntity
  ) where

import Yaifl.Prelude

-- | An object ID. Positive IDs are for `Yaifl.Thing.Kind`s and
-- negative IDs are for `Yaifl.Core.Kinds.Rooms`. This means we can write `Yaifl.Object.Kind.isThing` for free.
newtype Entity = Entity
  { unID :: Int
  } deriving stock (Show, Generic)
    deriving newtype (Eq, Read, Bounded, Hashable, Enum, Ord)

-- | Typeclass for extracting an entity from something (to store references).
class HasID n where
  getID :: n -> Entity

-- | Trivial instance.
instance HasID Entity where
  getID = id

-- | This should only be exposed in logs and tracing; i.e. the end user should never need to know there
-- is an ID system under it all.
instance Display Entity where
  displayBuilder i = "(ID: " <> show i <> ")"

-- | An entity tagged with a phantom @tagEntity@ for keeping some semblance of type safety
-- when indirectly storing references to other objects. The tagging mechanisms are in
-- `Yaifl.Tag`.
newtype TaggedEntity tagEntity = TaggedEntity { unTagEntity :: Entity }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Read, Bounded, Hashable, Enum, Ord)

-- | Tag an entity without a witness. This is, obviously, unsafe and probably should be avoided unless
-- you enjoy crashes.
unsafeTagEntity ::
  Entity
  -> TaggedEntity tagEntity
unsafeTagEntity = TaggedEntity

instance HasID (TaggedEntity t) where
  getID = unTagEntity

instance HasID (TaggedEntity e, o) where
  getID = unTagEntity . fst
-- | Phantom type for tagging `Yaifl.Thing.Kind`s.
data ThingTag
-- | Phantom type for tagging `Yaifl.Room.Kind`s.
data RoomTag
-- | Phantom type for tagging objects that enclose something (that have a `Yaifl.Enclosing.Kind` somewhere).
data EnclosingTag
-- | Phantom type for tagging doors (that have a `Yaifl.Door.Kind` somewhere).
data DoorTag
-- | Phantom type for tagging people (that have a `Yaifl.Person.Kind` somewhere).
data PersonTag

-- | Shorthand for enclosing entities.
type EnclosingEntity = TaggedEntity EnclosingTag
-- | Shorthand for room entities.
type RoomEntity = TaggedEntity RoomTag
-- | Shorthand for thing entities.
type ThingEntity = TaggedEntity ThingTag
-- | Shorthand for door entities.
type DoorEntity = TaggedEntity DoorTag
-- | Shorthand for person entities.
type PersonEntity = TaggedEntity PersonTag

makeFieldLabelsNoPrefix ''TaggedEntity
makeFieldLabelsNoPrefix ''Entity