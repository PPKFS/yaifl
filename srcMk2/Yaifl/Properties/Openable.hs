{-|
Module      : Yaifl.Properties.Openable
Description : A component for things that can be opened. 
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Properties.Openable 
  ( -- * Types
    Openable(..)
  ) where

import Solitude ( Eq, Ord, Read, Show, Generic )

-- | Whether the thing is open or not.
data Openable = Open | Closed 
  deriving stock (Eq, Show, Read, Ord, Generic)