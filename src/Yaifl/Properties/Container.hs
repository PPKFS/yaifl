{-|
Module      : Yaifl.Properties.Container
Description : A component for things that act as containers (this assumes containment rather than just enclosing.)
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Properties.Container 
  ( -- * Types
    Opacity(..)
  , Enterable(..)
  , Container(..)
    -- * Lenses
  , containerOpacity
  , containerEnclosing
  , containerOpenable
  , containerEnterable
  ) where

import Solitude ( Eq, Ord, Read, Show, Generic, makeLenses )
import Yaifl.Properties.Enclosing ( Enclosing )
import Yaifl.Properties.Openable ( Openable )

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