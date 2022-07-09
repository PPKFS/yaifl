-- ~\~ language=Haskell filename=src/Yaifl/Core/Properties/Openable.hs
-- ~\~ begin <<lit/properties/std/openable.md|src/Yaifl/Core/Properties/Openable.hs>>[0] project://lit/properties/std/openable.md:4
module Yaifl.Core.Properties.Openable 
  ( -- * Types
    Openable(..)
  ) where

import Solitude ( Eq, Ord, Read, Show, Generic )

-- | Whether the thing is open or not.
data Openable = Open | Closed 
  deriving stock (Eq, Show, Read, Ord, Generic)
-- ~\~ end
