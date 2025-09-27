{-|
Module      : Yaifl.Core.Entity
Copyright   : (c) Avery 2022-2025
License     : MIT
Maintainer  : ppkfs@outlook.com

An `Entity` is an object ID. DOCTODO.
-}

module Yaifl.Core.Entity
  ( -- * Entities
  -- | DOCTODO
    Entity(..)
  , HasID(..)
  -- ** TaggedEntity
  -- | Entities tagged with a phantom type that allows for additional type-safety when making indirect references.
  , TaggedEntity(unTag)
  , unsafeTagEntity
  -- ** Tags
  -- | These phantom types are defined here because these ones are foundational enough that they need to be
  -- forward-declared. That is, we need to be able to refer to type-safe `Entity`s at this level of the library
  -- that are also:
  -- `Yaifl.Core.Kinds.Room`s and `Yaifl.Core.Kinds.Thing`s (as these are two of the key types that everything else is based around);
  -- `Yaifl.Core.Kinds.Door`s and `Yaifl.Core.Kinds.Enclosing` (as these are part of the definition of `Yaifl.Core.Kinds.Room`)
  -- `Yaifl.Core.Kinds.Person` to store a reference to the current player in `Yaifl.Core.Metadata`.
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

-- | An object ID. Positive IDs are for `Yaifl.Core.Kinds.Thing`s and
-- negative IDs are for `Yaifl.Core.Kinds.Rooms`. This means we can write `Yaifl.Core.Kinds.Object` for free.
-- If you
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
-- `Yaifl.Core.Tag`.
newtype TaggedEntity tagEntity = TaggedEntity { unTag :: Entity }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Read, Bounded, Hashable, Enum, Ord)

-- | Tag an entity without a witness. This is, obviously, unsafe and probably should be avoided unless
-- you enjoy crashes.
unsafeTagEntity ::
  Entity
  -> TaggedEntity tagEntity
unsafeTagEntity = TaggedEntity

instance HasID (TaggedEntity t) where
  getID = unTag

instance HasID (TaggedEntity e, o) where
  getID = getID . fst

-- | Phantom type for tagging `Yaifl.Core.Kinds.Thing`s.
data ThingTag
-- | Phantom type for tagging `Yaifl.Core.Kinds.Room`s.
data RoomTag
-- | Phantom type for tagging objects that enclose something (that have a `Yaifl.Core.Kinds.Enclosing` somewhere).
data EnclosingTag
-- | Phantom type for tagging doors (that have a `Yaifl.Std.Kinds.Door` somewhere).
data DoorTag
-- | Phantom type for tagging people (that have a `Yaifl.Std.Kinds.Person` somewhere).
data PersonTag

-- | Shorthand for enclosing entities.
type EnclosingEntity = TaggedEntity EnclosingTag
-- | Shorthand for room entities.
type RoomEntity = TaggedEntity RoomTag
-- | Shorthand for thing entities.
type ThingEntity = TaggedEntity ThingTag
-- | Shorthand for door entities.
type DoorEntity = TaggedEntity DoorTag

type PersonEntity = TaggedEntity PersonTag