{-|
Module      : Yaifl.Core.Tag
Copyright   : (c) Avery 2023-2024
License     : MIT
Maintainer  : ppkfs@outlook.com

Machinery to tagEntity entities given a witness such that we can safely avoid Maybe when
doing queries. For example, we can require that the object reference of the object
that contains something is an object with an enclosing property.
-}

module Yaifl.Core.Tag
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
import Yaifl.Core.Entity

-- | A tagged object is a tuple of a tagged entity, and the object itself
newtype TaggedObject o tagEntity = TaggedObject { unTagObject :: (TaggedEntity tagEntity, o) }
  deriving stock (Generic)

instance HasID (TaggedObject o tagEntity) where
  getID = getID . fst . unTagObject

instance Display o => Display (TaggedObject o tag) where
  displayBuilder = displayBuilder . snd . unTagObject
-- | Unsafely tagEntity an object when we know it's sensible.
unsafeTagObject ::
  HasID o
  => o
  -> TaggedObject o tagEntity
unsafeTagObject o = TaggedObject (unsafeTagEntity $ getID o, o)

-- | An entity @e@ can be tagged as a `TaggedEntity` @taggableTo@ (a phantom type)
-- given a witness of type @taggableWith@.
class Taggable taggableWith taggableTo where
  tagEntity :: HasID e => taggableWith -> e -> TaggedEntity taggableTo
  default tagEntity :: HasID e => taggableWith -> e -> TaggedEntity taggableTo
  tagEntity _ = unsafeTagEntity . getID

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
  => HasID e
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
  Taggable (TaggedEntity a) b
  => TaggedEntity a
  -> TaggedEntity b
coerceTag a = tagEntity a (unTag a)