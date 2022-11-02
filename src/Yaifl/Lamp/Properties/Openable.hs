{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.Properties.Openable
  ( -- * Types
    Openable(..)
  , getOpenable
  ) where

import Yaifl.Core.Properties.TH
import Yaifl.Core.Objects.Query
import Yaifl.Core.Properties.Has
import Yaifl.Core.Properties.Query
import Solitude
import Effectful

-- | Whether the thing is open or not.
data Openable = Open | Closed
  deriving stock (Eq, Show, Read, Ord, Generic)

makeSpecificsWithout [] ''Openable
