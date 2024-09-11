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
  , makeContainer
  , inThe

  , getEnterableMaybe
  , setEnterable
  , modifyEnterable
  ) where

import Yaifl.Prelude

import Yaifl.Model.Effects
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Model.HasProperty ( WMWithProperty )
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Query ( defaultPropertySetter, defaultPropertyGetter, modifyProperty )
import Yaifl.Model.TH ( makeSpecificsWithout )
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Entity
import Yaifl.Model.Tag

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

isOpaqueClosedContainer ::
  Container
  -> Bool
isOpaqueClosedContainer c = (opacity c == Opaque) && (view #openable c == defaultContainerOpenability)

makeContainer ::
  Maybe Int
  -> Maybe Opacity
  -> Maybe Enterable
  -> Maybe Openable
  -> Maybe Opened
  -> Container
makeContainer cc op e oa opd = (Container Opaque (blankEnclosing { capacity = Just 100 }) defaultContainerOpenability NotEnterable)
  & (maybe id (set #enterable) e)
  & (maybe id (set (#enclosing % #capacity) . Just) cc)
  & (maybe id (set #opacity) op)
  & (maybe id (set (#openable % #opened)) opd)
  & (maybe id (set (#openable % #openable)) oa)

data ContainerTag
type ContainerEntity = TaggedEntity ContainerTag

inThe ::
  ContainerEntity
  -> EnclosingEntity
inThe = coerceTag

instance Taggable (TaggedEntity ContainerTag) EnclosingTag
instance Taggable Container ContainerTag