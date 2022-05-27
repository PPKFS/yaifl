-- ~\~ language=Haskell filename=src/Yaifl/Properties/Container.hs
-- ~\~ begin <<lit/properties/std/container.md|src/Yaifl/Properties/Container.hs>>[0] project://lit/properties/std/container.md:4

{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Properties.Container 
  ( -- * Types
    Opacity(..)
  , Enterable(..)
  , Container(..)

  , isOpaqueClosedContainer
    -- * Lenses
  , containerOpacity
  , containerEnclosing
  , containerOpenable
  , containerEnterable
  ) where

import Solitude
import Yaifl.Properties.Enclosing ( Enclosing )
import Yaifl.Properties.Openable ( Openable(..) )

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

isOpaqueClosedContainer :: 
  Container
  -> Bool
isOpaqueClosedContainer c = (_containerOpacity c == Opaque) && (_containerOpenable c == Closed)
-- ~\~ end
