{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.Properties.Container
  ( -- * Types
    Opacity(..)
  , Enterable(..)
  , Container(..)

  , getContainer
  , setContainer
  , modifyContainer
  , isOpaqueClosedContainer

  , getEnterable
  , setEnterable
  , modifyEnterable
    -- * Lenses
  , containerOpacity
  , containerEnclosing
  , containerOpenable
  , containerEnterable
  ) where

import Solitude

import Yaifl.Core.Objects.Query ( ObjectLike, NoMissingObjects )
import Yaifl.Core.Properties.Enclosing ( Enclosing )
import Yaifl.Core.Properties.Has ( WMHasProperty )
import Yaifl.Core.Properties.Query ( defaultPropertySetter, defaultPropertyGetter, modifyProperty )
import Yaifl.Core.Properties.TH ( makeSpecificsWithout )
import Yaifl.Lamp.Properties.Openable ( Openable(..) )

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