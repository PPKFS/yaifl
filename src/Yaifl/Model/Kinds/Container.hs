module Yaifl.Model.Kinds.Container
  ( -- * Types
    Opacity(..)
  , Enterable(..)
  , Container(..)

  , getContainerMaybe
  , setContainer
  , modifyContainer
  , isOpaqueClosedContainer

  , getEnterableMaybe
  , setEnterable
  , modifyEnterable
    -- * Lenses
  , containerOpacity
  , containerEnclosing
  , containerOpenable
  , containerEnterable
  ) where

import Solitude

import Yaifl.Model.Effects
import Yaifl.Model.Kinds.Enclosing ( Enclosing )
import Yaifl.Model.HasProperty ( WMWithProperty )
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Query ( defaultPropertySetter, defaultPropertyGetter, modifyProperty )
import Yaifl.Model.TH ( makeSpecificsWithout )
import Yaifl.Model.Kinds.AnyObject

-- | If the container is see-through.
data Opacity = Opaque | Transparent
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | If the container is enterable (by a person or animal or other sentient being).
data Enterable = Enterable | NotEnterable
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | A container.
data Container = Container
  { _containerOpacity :: Opacity
  , _containerEnclosing :: Enclosing
  , _containerOpenable :: Openability
  , _containerEnterable :: Enterable
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeLenses ''Container
makeSpecificsWithout [] ''Container
makeSpecificsWithout [] ''Enterable

isOpaqueClosedContainer ::
  Container
  -> Bool
isOpaqueClosedContainer c = (_containerOpacity c == Opaque) && (_containerOpenable c == defaultContainerOpenability)