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

import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Std.Kinds.Container
import Yaifl.Enclosing.Kind
import Yaifl.Object.Kind
import Yaifl.Metadata
import Yaifl.Core.Query.Property
import Yaifl.TH
import Yaifl.Tag
import Yaifl.Thing.Kind ( Thing )
import Yaifl.Core.Query.Enclosing
import Yaifl.ObjectLike

data Supporter = Supporter
  { enclosing :: Enclosing
  , enterable :: Enterable
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | Check if @o@ is of the @supporter@ type.
isSupporter ::
  WithoutMissingObjects wm es
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