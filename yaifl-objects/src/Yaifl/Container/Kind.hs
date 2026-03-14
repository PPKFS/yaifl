module Yaifl.Container.Kind
  ( -- * Types
    Opacity(..)
  , Enterable(..)
  , Container(..)
  , ContainerEntity
  , ContainerTag

  , getContainerMaybe
  , getEnterableMaybe
  , isOpaqueClosedContainer
  , makeContainer
  , inThe

  , isOpenContainer
  , isEmptyContainer
  , isClosedContainer
  , isLockedContainer
  , isEmptyTransparentContainer
  , isTransparentContainer
  , isOpenableContainer
  , isOpenTransparentContainer
  , isOpaqueContainer
  , TaggedContainer
  ) where


import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.AnyObject
import Yaifl.Enclosing.Kind
import Yaifl.Thing.Kind
import Yaifl.Enclosing.Query
import Yaifl.Property.Query( defaultPropertyGetter )
import Yaifl.TH ( WMWithProperty, makeGetMaybe )
import Yaifl.Tag
import Yaifl.Openable.Kind
import qualified Data.EnumSet as ES

-- | If the container is see-through.
data Opacity = Opaque | Transparent
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | If the container is enterable (by a person or animal or other sentient being).
data Enterable = Enterable | NotEnterable
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | A container.
data Container = Container
  { opacity :: Opacity
  , enclosing :: Enclosing
  , openable :: Openability
  , enterable :: Enterable
  } deriving stock (Eq, Show, Read, Ord, Generic)

data ContainerTag
type ContainerEntity = TaggedEntity ContainerTag

type TaggedContainer wm = TaggedObject (Thing wm) ContainerTag

inThe ::
  ContainerEntity
  -> EnclosingEntity
inThe = coerceTag

instance Taggable ContainerEntity EnclosingTag
instance Taggable Container EnclosingTag
instance Taggable Container ContainerTag

instance IsEnclosing ContainerEntity

makeFieldLabelsNoPrefix ''Container
makeFieldLabelsNoPrefix ''Enterable

makeGetMaybe ''Container
makeGetMaybe ''Enterable

isOpenContainer ::
  Container
  -> Bool
isOpenContainer = (== Open) . view (#openable % #opened)

isOpenableContainer ::
  Container
  -> Bool
isOpenableContainer = (== Openable) . view (#openable % #openable)

isClosedContainer ::
  Container
  -> Bool
isClosedContainer = (== Closed) . view (#openable % #opened)

isLockedContainer ::
  Container
  -> Bool
isLockedContainer = (== Closed) . view (#openable % #opened)

isEmptyContainer ::
  Container
  -> Bool
isEmptyContainer = ES.null . view (#enclosing % #contents)

isEmptyTransparentContainer ::
  Container
  -> Bool
isEmptyTransparentContainer c = isEmptyContainer c && isTransparentContainer c

isTransparentContainer ::
  Container
  -> Bool
isTransparentContainer = (== Transparent) . view #opacity

isOpaqueContainer ::
  Container
  -> Bool
isOpaqueContainer = (== Opaque) . view #opacity

isOpenTransparentContainer ::
  Container
  -> Bool
isOpenTransparentContainer c = isOpenContainer c && isTransparentContainer c

isOpaqueClosedContainer ::
  Container
  -> Bool
isOpaqueClosedContainer c = isClosedContainer c && isOpaqueContainer c

makeContainer ::
  Maybe Int
  -> Maybe Opacity
  -> Maybe Enterable
  -> Maybe Openable
  -> Maybe Opened
  -> Container
makeContainer cc op e oa opd = (Container
  { opacity = fromMaybe Opaque op
  , enclosing = (blankEnclosing { capacity = cc <|> Just 100 })
  , openable = defaultContainerOpenability
  , enterable = fromMaybe NotEnterable e
  })
  & maybe id (set (#openable % #opened)) opd
  & maybe id (set (#openable % #openable)) oa
