{-|
Module      : Yaifl.Model.Properties.Supporter
Copyright   : (c) Avery 2022-2024
License     : MIT
Maintainer  : ppkfs@outlook.com

A property for things which can act as a supporter for other things (a container without the physical
enclosing).
-}

module Yaifl.Model.Kinds.Supporter
  ( isSupporter
  , SupporterEntity
  , onThe
  , SupporterTag
  , Supporter(..)
  , getSupporterMaybe
  , SupporterThing
  ) where

import Yaifl.Prelude

import Yaifl.Model.Effects
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Metadata
import Yaifl.Model.Query
import Yaifl.Model.TH
import Yaifl.Model.Tag
import Yaifl.Model.Kinds.Thing ( Thing )

data Supporter = Supporter
  { enclosing :: Enclosing
  , enterable :: Enterable
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | Check if @o@ is of the @supporter@ type.
isSupporter ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isSupporter o = getObject o >>= (`isKind` "supporter")

data SupporterTag
type SupporterEntity = TaggedEntity SupporterTag
type SupporterThing wm = TaggedObject (Thing wm) SupporterTag

onThe ::
  SupporterEntity
  -> EnclosingEntity
onThe = coerceTag

instance Taggable Supporter SupporterTag
instance Taggable (TaggedEntity SupporterTag) EnclosingTag

makeFieldLabelsNoPrefix ''Supporter
makeSpecificsWithout [] ''Supporter