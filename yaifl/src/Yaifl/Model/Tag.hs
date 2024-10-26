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
  , TaggedObject(..)
  , unsafeTagObject
  , getTaggedObject
  , tagObject
  ) where

import Yaifl.Prelude
import Yaifl.Model.Entity

{-
A tagged entity can do getX
which means a tagged object can also do getX

-}
-- | A tagged object is a tuple of a tagged entity, and the object itself
newtype TaggedObject o tag = TaggedObject { unTagObject :: (TaggedEntity tag, o) }
  deriving stock (Generic)

instance HasID (TaggedObject o tag) where
  getID = getID . fst . unTagObject

-- | Unsafely tag an object when we know it's sensible.
unsafeTagObject ::
  HasID o
  => o
  -> TaggedObject o tag
unsafeTagObject o = TaggedObject (unsafeTagEntity $ getID o, o)

instance TaggedAs (TaggedObject o tag) tag where
  toTag = fst . unTagObject

-- | An entity @e@ can be tagged as a `TaggedEntity` @taggableTo@ (a phantom type)
-- given a witness of type @taggableWith@.
class Taggable taggableWith taggableTo where
  tag :: HasID e => taggableWith -> e -> TaggedEntity taggableTo
  default tag :: HasID e => taggableWith -> e -> TaggedEntity taggableTo
  tag _ = unsafeTagEntity . getID

-- you can always tag something as itself
instance Taggable (TaggedEntity a) a

getTaggedObject ::
  TaggedObject o tag
  -> o
getTaggedObject = snd . unTagObject

tagObject ::
  Taggable tagWith taggableTo
  => HasID e
  => tagWith
  -> e
  -> TaggedObject e taggableTo
tagObject ev e = TaggedObject (tag ev e, e)

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