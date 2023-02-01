{-|
Module      : Yaifl.Core.Properties.Enclosing
Description : A component that holds things.
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

This is in the core library because it is required (e.g. for location info, and for rooms).
It is agnostic as to what is doing the containing, so rooms, containers, supporters, etc can all
be queried in the same way.
-}

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
