-- ~\~ language=Haskell filename=src/Yaifl/Core/Properties/Container.hs
-- ~\~ begin <<lit/properties/std/container.md|src/Yaifl/Core/Properties/Container.hs>>[0] project://lit/properties/std/container.md:4

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


import Yaifl.Core.Properties.Enclosing ( Enclosing )
import Yaifl.Lamp.Properties.Openable ( Openable(..) )
import Yaifl.Core.Properties.TH
import Yaifl.Core.Logger
import Yaifl.Core.Objects.Query
import Yaifl.Core.Properties.Property
import Yaifl.Core.Properties.Query
import Yaifl.Core.Common

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
-- ~\~ end
