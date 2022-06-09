# Openable

```haskell file=src/Yaifl/Properties/Openable.hs
module Yaifl.Properties.Openable 
  ( -- * Types
    Openable(..)
  ) where

import Solitude ( Eq, Ord, Read, Show, Generic )

-- | Whether the thing is open or not.
data Openable = Open | Closed 
  deriving stock (Eq, Show, Read, Ord, Generic)
```