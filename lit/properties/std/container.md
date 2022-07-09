# Container

```haskell file=src/Yaifl/Lamp/Properties/Container.hs

{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Properties.Container 
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


import Yaifl.Core.Properties.Enclosing ( Enclosing )
import Yaifl.Core.Properties.Openable ( Openable(..) )

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
```
