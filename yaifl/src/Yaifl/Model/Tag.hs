{-|
Module      : Yaifl.Model.Objects.Tag
Copyright   : (c) Avery 2023
License     : MIT
Maintainer  : ppkfs@outlook.com

Machinery to tag entities given a witness such that we can safely avoid Maybe when
doing queries. For example, we can require that the object reference of the object
that contains something is an object with an enclosing property.
-}
module Yaifl.Model.Tag
  ( -- * Tagging things
    Taggable(..)
  , TaggedAs(..)
  , coerceTag
   -- ** Tagging
  , TaggedObject(unTagObject)
  , unsafeTagObject
  ) where

import Yaifl.Prelude
import Yaifl.Model.Entity

-- | Because tagging object entities is a headache to marshall between `AnyObject` and not, this is an Object
-- level equivalent to `TaggedEntity`; it allows for witnessed property lookups without `Maybe`.
-- (e.g. a `TaggedObject (Thing wm) DoorTag` can have `Yaifl.Model.Properties.Door.getDoor` rather than `Yaifl.Model.Properties.Door.getDoorMaybe`).
newtype TaggedObject o tag = TaggedObject { unTagObject :: o }
  deriving stock (Generic)

instance HasID o => HasID (TaggedObject o tag) where
  getID = getID . unTagObject

-- | Unsafely tag an object when we know it's sensible.
unsafeTagObject ::
  o
  -> TaggedObject o tag
unsafeTagObject = TaggedObject

instance HasID o => TaggedAs (TaggedObject o tag) tag where
  toTag = unsafeTagEntity . getID . unTagObject

-- | An entity @e@ can be tagged as a `TaggedEntity` @taggableTo@ (a phantom type)
-- given a witness of type @taggableWith@.
class Taggable taggableWith taggableTo where
  tag :: HasID e => taggableWith -> e -> TaggedEntity taggableTo
  default tag :: HasID e => taggableWith -> e -> TaggedEntity taggableTo
  tag _ = unsafeTagEntity . getID

-- | An @a@ that is already tagged does not need a witness.
class TaggedAs a taggedAs where
  toTag :: a -> TaggedEntity taggedAs

instance TaggedAs (TaggedEntity a) a where
  toTag = id

-- | Weakening the tag on a room to an enclosing
instance Taggable (TaggedEntity RoomTag) EnclosingTag where
  tag = const . coerce

instance TaggedAs (TaggedEntity RoomTag) EnclosingTag where
  toTag = coerce

instance TaggedAs (TaggedEntity DoorTag) ThingTag where
  toTag = coerce

-- | If we can tag a `TaggedEntity a` as a @b@, we can just coerce the entity
-- rather than passing it twice.
coerceTag ::
  Taggable (TaggedEntity a) b
  => TaggedEntity a
  -> TaggedEntity b
coerceTag a = tag a (unTag a)