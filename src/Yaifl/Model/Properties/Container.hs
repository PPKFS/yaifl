{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Model.Properties.Container
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

import Yaifl.Model.Properties.Enclosing ( Enclosing )
import Yaifl.Model.Properties.Has ( WMWithProperty )
import Yaifl.Model.Properties.Query ( defaultPropertySetter, defaultPropertyGetter, modifyProperty )
import Yaifl.Model.Properties.TH ( makeSpecificsWithout )
import Yaifl.Model.Properties.Openable ( Openable(..) )
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Object

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
  , _containerOpenable :: Openable
  , _containerEnterable :: Enterable
  } deriving stock (Eq, Show, Read, Ord, Generic)

makeLenses ''Container
makeSpecificsWithout [] ''Container
makeSpecificsWithout [] ''Enterable

isOpaqueClosedContainer ::
  Container
  -> Bool
isOpaqueClosedContainer c = (_containerOpacity c == Opaque) && (_containerOpenable c == Closed)