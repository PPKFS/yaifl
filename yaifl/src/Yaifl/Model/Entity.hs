{-|
Module      : Yaifl.Model.Entity
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

Object IDs.
-}

module Yaifl.Model.Entity
  ( -- * Entities
    Entity(..)
  , HasID(..)
  , TaggedEntity(unTag)
  , unsafeTagEntity
  -- ** Tags
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

-- | An object ID.
newtype Entity = Entity
  { unID :: Int
  } deriving stock (Show, Generic)
    deriving newtype (Eq, Num, Read, Bounded, Hashable, Enum, Ord, Real, Integral)

-- | Typeclass and function for extracting an entity from something (to store references)
class HasID n where
  -- | Get an ID.
  getID :: n -> Entity

-- | Trivial instance.
instance HasID Entity where
  getID = id

-- | For pretty printing in logs.
instance Display Entity where
  displayBuilder i = "(ID: " <> show i <> ")"

-- | An entity tagged with a phantom @tagEntity@ for keeping some semblance of type safety
-- when indirectly storing references to other objects. The tagging mechanisms are in
-- @Yaifl.Model.Objects.Tag@.
newtype TaggedEntity tagEntity = TaggedEntity { unTag :: Entity }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Num, Read, Bounded, Hashable, Enum, Ord, Real, Integral)

-- | Tag an entity without a witness.
unsafeTagEntity ::
  Entity -- ^ Entity to tagEntity
  -> TaggedEntity tagEntity
unsafeTagEntity = TaggedEntity

instance HasID (TaggedEntity t) where
  getID = unTag

instance HasID (TaggedEntity e, o) where
  getID = getID . fst

-- | Phantom type for tagging `Yaifl.Model.Objects.Thing`s.
data ThingTag
-- | Phantom type for tagging `Yaifl.Model.Objects.Room`s.
data RoomTag
-- | Phantom type for tagging objects that enclose something.
data EnclosingTag
-- | Phantom type for tagging doors.
data DoorTag

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