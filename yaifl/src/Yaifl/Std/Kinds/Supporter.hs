{-|
Module      : Yaifl.Model.Properties.Supporter
Copyright   : (c) Avery 2022-2024
License     : MIT
Maintainer  : ppkfs@outlook.com

A property for things which can act as a supporter for other things (a container without the physical
enclosing).
-}

module Yaifl.Std.Kinds.Supporter
  ( isSupporter
  , SupporterEntity
  , onThe
  , SupporterTag
  , Supporter(..)
  , getSupporterMaybe
  , SupporterThing
  ) where

import Yaifl.Prelude

import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Std.Kinds.Container
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Metadata
import Yaifl.Core.Query.Property
import Yaifl.Core.TH
import Yaifl.Core.Tag
import Yaifl.Core.Kinds.Thing ( Thing )
import Yaifl.Core.Query.Enclosing
import Yaifl.Core.ObjectLike

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

instance IsEnclosing SupporterEntity

makeFieldLabelsNoPrefix ''Supporter
makeSpecificsWithout [] ''Supporter