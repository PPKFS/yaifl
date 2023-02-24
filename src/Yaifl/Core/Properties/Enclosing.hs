{-# LANGUAGE StrictData #-}

module Yaifl.Core.Properties.Enclosing (
  -- * Enclosing
    Enclosing(..)
  , blankEnclosing
  ) where

import Solitude

import Data.EnumSet ( EnumSet, empty )
import Yaifl.Core.Entity ( Entity )

-- | A component that contains other objects.
data Enclosing = Enclosing
  { contents :: EnumSet Entity -- ^ The contained objects.
  , capacity :: Maybe Int -- ^ An optional number of items that can be contained.
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | An enclosing component with nothing in it.
blankEnclosing :: Enclosing
blankEnclosing = Enclosing Data.EnumSet.empty Nothing
