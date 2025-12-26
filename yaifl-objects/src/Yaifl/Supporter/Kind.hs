module Yaifl.Supporter.Kind
  ( SupporterEntity
  , onThe
  , SupporterTag
  , Supporter(..)
  , getSupporterMaybe
  , SupporterThing
  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Container.Kind
import Yaifl.Enclosing.Kind
import Yaifl.Property.Query
import Yaifl.TH
import Yaifl.Tag
import Yaifl.Thing.Kind ( Thing )
import Yaifl.Enclosing.Query

data Supporter = Supporter
  { enclosing :: Enclosing
  , enterable :: Enterable
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeFieldLabelsNoPrefix ''Supporter
makeGetMaybe ''Supporter

data SupporterTag
type SupporterEntity = TaggedEntity SupporterTag
type SupporterThing wm = TaggedObject (Thing wm) SupporterTag

onThe ::
  SupporterEntity
  -> EnclosingEntity
onThe = coerceTag

instance Taggable Supporter SupporterTag
instance Taggable (TaggedEntity SupporterTag) EnclosingTag

instance IsEnclosing SupporterEntity
