# Enclosing

```haskell file=src/Yaifl/Core/Properties/Enclosing.hs
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Properties.Enclosing 
  ( -- * Types
    Enclosing(..)
  , blankEnclosing
  
    -- * Lenses
  , enclosingContains
  , enclosingCapacity
  ) where

import Solitude hiding (empty)
import Data.EnumSet (EnumSet, empty)
import Yaifl.Core.Common (Entity)

-- | A component that contains other objects.
data Enclosing = Enclosing
  { _enclosingContains :: EnumSet Entity
  , _enclosingCapacity :: Maybe Int
  } deriving stock (Eq, Show, Read, Ord, Generic)

blankEnclosing :: Enclosing
blankEnclosing = Enclosing empty Nothing

makeLenses ''Enclosing
```
