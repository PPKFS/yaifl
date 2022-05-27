-- ~\~ language=Haskell filename=src/Yaifl/Properties/Enclosing.hs
-- ~\~ begin <<lit/properties/std/enclosing.md|src/Yaifl/Properties/Enclosing.hs>>[0] project://lit/properties/std/enclosing.md:4
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Properties.Enclosing 
  ( -- * Types
    Enclosing(..)
  , blankEnclosing
  
    -- * Lenses
  , enclosingContains
  , enclosingCapacity
  ) where

import Solitude hiding (empty)
import Data.EnumSet (EnumSet, empty)
import Yaifl.Common (Entity)

-- | A component that contains other objects.
data Enclosing = Enclosing
  { _enclosingContains :: EnumSet Entity
  , _enclosingCapacity :: Maybe Int
  } deriving stock (Eq, Show, Read, Ord, Generic)

blankEnclosing :: Enclosing
blankEnclosing = Enclosing empty Nothing

makeLenses ''Enclosing
-- ~\~ end
