module Yaifl.Model.Kinds.Container
  ( -- * Types
    Opacity(..)
  , Enterable(..)
  , Container(..)
  , ContainerEntity
  , ContainerTag

  , getContainerMaybe
  , setContainer
  , modifyContainer
  , isOpaqueClosedContainer
  , isContainer
  , makeContainer
  , inThe

  , getEnterableMaybe
  , setEnterable
  , modifyEnterable
  , thingIsOpenContainer
  , thingIsClosedContainer
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

import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Core.HasProperty ( WMWithProperty )
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Query ( defaultPropertySetter, defaultPropertyGetter, modifyProperty, ObjectLike (..), IsEnclosing )
import Yaifl.Model.TH ( makeSpecificsWithout )
import Yaifl.Core.Tag
import Yaifl.Core.Kinds.Object
import qualified Data.EnumSet as ES
import Yaifl.Core.Metadata
import Yaifl.Model.Kinds (Thing)

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

makeFieldLabelsNoPrefix ''Container
makeSpecificsWithout [] ''Container
makeSpecificsWithout [] ''Enterable

-- | Check if @o@ is of the @container@ type.
isContainer ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isContainer o = getObject o >>= (`isKind` "container")

isOpaqueClosedContainer ::
  Container
  -> Bool
isOpaqueClosedContainer c = isClosedContainer c && isOpaqueContainer c

thingIsOpenContainer ::
  WMWithProperty wm Container
  => Thing wm
  -> Bool
thingIsOpenContainer = (== Just Open) . fmap (view (#openable % #opened)) . getContainerMaybe

thingIsClosedContainer ::
  WMWithProperty wm Container
  => Thing wm
  -> Bool
thingIsClosedContainer = (== Just Closed) . fmap (view (#openable % #opened)) . getContainerMaybe

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