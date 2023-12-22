module Yaifl.Model.Objects.Tag
  ( -- * Tagging things
    Taggable(..)
  , coerceTag
  ) where

import Solitude
import Yaifl.Model.Objects.Entity

-- | An entity @e@ can be tagged as a `TaggedEntity` @taggableTo@ (a phantom type)
-- given a witness of type @taggableWith@.
class Taggable taggableWith taggableTo where
  tag :: HasID e => taggableWith -> e -> TaggedEntity taggableTo
  default tag :: HasID e => taggableWith -> e -> TaggedEntity taggableTo
  tag _ = unsafeTagEntity . getID

-- | Weakening the tag on a room to an enclosing
instance Taggable (TaggedEntity RoomTag) EnclosingTag where
  tag = const . coerce

-- | If we can tag a `TaggedEntity a` as a `b`, we can just coerce the entity
-- rather than passing it twice.
coerceTag ::
  Taggable (TaggedEntity a) b
  => TaggedEntity a
  -> TaggedEntity b
coerceTag a = tag a (unTag a)