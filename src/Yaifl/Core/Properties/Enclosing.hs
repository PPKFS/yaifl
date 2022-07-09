-- ~\~ language=Haskell filename=src/Yaifl/Core/Properties/Enclosing.hs
-- ~\~ begin <<lit/properties/std/enclosing.md|src/Yaifl/Core/Properties/Enclosing.hs>>[0] project://lit/properties/std/enclosing.md:4
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

import Data.EnumSet (EnumSet, empty)
import Yaifl.Core.Common (Entity)

-- | A component that contains other objects.
data Enclosing = Enclosing
  { _enclosingContains :: EnumSet Entity
  , _enclosingCapacity :: Maybe Int
  } deriving stock (Eq, Show, Read, Ord, Generic)

blankEnclosing :: Enclosing
blankEnclosing = Enclosing Data.EnumSet.empty Nothing

makeLenses ''Enclosing
-- ~\~ end
