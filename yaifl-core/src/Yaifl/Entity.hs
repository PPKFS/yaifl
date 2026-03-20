{-|
Module      : Yaifl.Entity
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

An `Entity` is a unique identifier for game objects, wrapping an `Int`.
Positive IDs are for `Yaifl.Thing.Kind.Thing`s, negative for `Yaifl.Room.Kind.Room`s.
Generate entities via `Yaifl.Effects.ObjectQuery.generateEntity` for global uniqueness.

`Entity` can also serve as a key for `Yaifl.Store`, treated as an opaque identifier.
For type-safe references, use `TaggedEntity` with phantom types (see `Yaifl.Tag`).
-}

module Yaifl.Entity
  ( -- * Entities
    Entity(..)
  , HasEntity(..)
  -- ** TaggedEntity
  -- | Entities tagged with a phantom type for additional type-safety when making indirect references.
  , TaggedEntity(unTagEntity)
  , unsafeTagEntity
  -- ** Tags
  -- | Phantom types for type-safe references to core types.
  -- `EnclosingTag` is forward-declared for the containment relation on `Thing`s.
  -- `DoorTag` is forward-declared for the connection between `Room`s potentially having a door.
  -- `PersonTag` is forward-declared to keep track of the current player.
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
-- negative IDs are for `Yaifl.Room.Kind.Room`s. This means we can write `Yaifl.Object.Kind.isThing` for free.
newtype Entity = Entity
  { unEntity :: Int
  } deriving stock (Show, Generic)
    deriving newtype (Eq, Read, Bounded, Hashable, Enum, Ord)

-- | Typeclass for extracting an entity from something (to store references).
class HasEntity n where
  getEntity :: n -> Entity

-- | Trivial instance.
instance HasEntity Entity where
  getEntity = id

-- | `Display` instance for debugging. Format: @(ID: <number>)@.
instance Display Entity where
  displayBuilder i = "(ID: " <> show i <> ")"

-- | An entity tagged with a phantom type for type-safe references.
newtype TaggedEntity tagEntity = TaggedEntity { unTagEntity :: Entity }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Read, Bounded, Hashable, Enum, Ord)

-- | Tag an entity without a witness. Unsafe; use only in controlled contexts.
-- Prefer `Yaifl.Tag.tagEntity` for type-safe tagging.
unsafeTagEntity ::
  Entity
  -> TaggedEntity tagEntity
unsafeTagEntity = TaggedEntity

instance HasEntity (TaggedEntity t) where
  getEntity = unTagEntity

-- | Phantom type for tagging objects that can be treated as a `Yaifl.Thing.Kind.Thing`.
data ThingTag
-- | For tagged things.
type ThingEntity = TaggedEntity ThingTag

-- | Phantom type for tagging objects that can be treated as a `Yaifl.Room.Kind.Room`.
data RoomTag
-- | For tagged rooms.
type RoomEntity = TaggedEntity RoomTag

-- | Phantom type for tagging objects that can be treated as an enclosing (that have a `Yaifl.Enclosing.Kind.Enclosing` somewhere).
data EnclosingTag
-- | For tagged enclosings.
type EnclosingEntity = TaggedEntity EnclosingTag

-- | Phantom type for tagging objects that can be treated as a door (that have a `Yaifl.Door.Kind.Door` somewhere).
data DoorTag
-- | For tagged doors.
type DoorEntity = TaggedEntity DoorTag

-- | Phantom type for tagging objects that can be treated as a person (that have a `Yaifl.Person.Kind.Person` somewhere).
data PersonTag
-- | For tagged people.
type PersonEntity = TaggedEntity PersonTag

