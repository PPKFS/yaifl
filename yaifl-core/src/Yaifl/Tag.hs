{-|
Module      : Yaifl.Tag
Copyright   : (c) Avery 2023-2024
License     : MIT
Maintainer  : ppkfs@outlook.com

Machinery to tagEntity entities given a witness such that we can safely avoid Maybe when
doing queries. For example, we can require that the object reference of the object
that contains something is an object with an enclosing property.
-}

module Yaifl.Tag
  ( -- * Tagging things
    Taggable(..)
  , coerceTag
   -- ** Tagging
  , TaggedObject
  , unTagObject
  , unsafeTagObject
  , getTaggedObject
  , getTag
  , tagObject
  ) where

import Yaifl.Prelude
import Yaifl.Entity

-- | A tagged object is a tuple of a tagged entity, and the object itself
newtype TaggedObject o tagEntity = TaggedObject { unTagObject :: (TaggedEntity tagEntity, o) }
  deriving stock (Generic)

instance HasEntity (TaggedObject o tagEntity) where
  getEntity = getEntity . fst . unTagObject

instance Display o => Display (TaggedObject o tag) where
  displayBuilder = displayBuilder . snd . unTagObject

-- | Unsafely tagEntity an object when we know it's sensible.
unsafeTagObject ::
  HasEntity o
  => o
  -> TaggedObject o tagEntity
unsafeTagObject o = TaggedObject (unsafeTagEntity $ getEntity o, o)

-- | An entity @e@ can be tagged as a `TaggedEntity` @taggableTo@ (a phantom type)
-- given a witness of type @taggableWith@.
class Taggable taggableWith taggableTo where
  tagEntity :: HasEntity e => taggableWith -> e -> TaggedEntity taggableTo
  default tagEntity :: HasEntity e => taggableWith -> e -> TaggedEntity taggableTo
  tagEntity _ = unsafeTagEntity . getEntity

-- | you can always tagEntity something as itself
instance Taggable (TaggedEntity a) a
instance Taggable DoorEntity ThingTag

instance Taggable (TaggedObject o tag) tag

getTaggedObject ::
  TaggedObject o tagEntity
  -> o
getTaggedObject = snd . unTagObject

getTag ::
  TaggedObject o tagEntity
  -> TaggedEntity tagEntity
getTag = fst . unTagObject

tagObject ::
  Taggable tagWith taggableTo
  => HasEntity e
  => tagWith
  -> e
  -> TaggedObject e taggableTo
tagObject ev e = TaggedObject (tagEntity ev e, e)

-- | Weakening the tagEntity on a room to an enclosing
instance Taggable (TaggedEntity RoomTag) EnclosingTag where
  tagEntity = const . coerce

instance Taggable (TaggedEntity PersonTag) EnclosingTag
instance Taggable (TaggedEntity PersonTag) ThingTag

-- | If we can tagEntity a `TaggedEntity a` as a @b@, we can just coerce the entity
-- rather than passing it twice.
coerceTag ::
  forall b a.
  Taggable (TaggedEntity a) b
  => TaggedEntity a
  -> TaggedEntity b
coerceTag a = tagEntity a (unTagEntity a)