# Openable

```haskell file=src/Yaifl/Core/Properties/Openable.hs
module Yaifl.Core.Properties.Openable 
  ( -- * Types
    Openable(..)
  ) where

 ( Eq, Ord, Read, Show, Generic )

-- | Whether the thing is open or not.
data Openable = Open | Closed 
  deriving stock (Eq, Show, Read, Ord, Generic)
```
