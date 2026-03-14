{-|
Module      : Yaifl.Entity
Copyright   : (c) Avery 2022-2025
License     : MIT
Maintainer  : ppkfs@outlook.com

An `Entity` is an object ID used to uniquely identify game objects. It is a simple wrapper around an `Int`,
where positive IDs are implicitly used for `Yaifl.Thing.Kind.Thing`s and negative IDs for `Yaifl.Room.Kind.Room`s.
Entities should only be generated via `Yaifl.Effects.ObjectQuery.generateEntity` to ensure global uniqueness.

`Entity` can also serve as a key for `Yaifl.Store`, where it is treated as an opaque identifier.
For type-safe references, use `TaggedEntity` with phantom types (see `Yaifl.Tag`).
-}

module Yaifl.Entity
  ( -- * Entities
    Entity(..)
  , HasEntity(..)
  -- ** TaggedEntity
  -- | Entities tagged with a phantom type that allows for additional type-safety when making indirect references.
  , TaggedEntity(unTagEntity)
  , unsafeTagEntity
  -- ** Tags
  -- | These phantom types are defined here because they are foundational and need to be forward-declared.
  -- They are used to create type-safe references to core types like `Yaifl.Room.Kind.Room` and
  -- `Yaifl.Thing.Kind.Thing`. The tagging machinery (e.g., `tagEntity`) is defined in `Yaifl.Tag`.
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

-- | `Display` instance for debugging and logging purposes. This instance is intended for internal use only
-- and should not be exposed to end users. The output format is @(ID: <number>)@, where @<number>@ is the
-- underlying `Int` value of the `Entity`.
instance Display Entity where
  displayBuilder i = "(ID: " <> show i <> ")"

-- | An entity tagged with a phantom @tagEntity@ for keeping some semblance of type safety
-- when indirectly storing references to other objects. For tagging mechanisms, see `Yaifl.Tag`.
newtype TaggedEntity tagEntity = TaggedEntity { unTagEntity :: Entity }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Read, Bounded, Hashable, Enum, Ord)

-- | Tag an entity without a witness. This is unsafe and should only be used in controlled contexts
-- (e.g., testing, initialization, or legacy code), as it bypasses type safety checks and can lead to
-- runtime errors or undefined behavior if misused.
--
-- For type-safe tagging, use `Yaifl.Tag.tagEntity` with a proper witness.
unsafeTagEntity ::
  Entity
  -> TaggedEntity tagEntity
unsafeTagEntity = TaggedEntity

instance HasEntity (TaggedEntity t) where
  getEntity = unTagEntity
-- | Phantom type for tagging objects that can be treated as a thing (that have a `Yaifl.Thing.Kind.Thing` somewhere).
data ThingTag
-- | For tagged things.
type ThingEntity = TaggedEntity ThingTag

-- | Phantom type for tagging objects that can be treated as a room (that have a `Yaifl.Room.Kind.Room` somewhere).
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

