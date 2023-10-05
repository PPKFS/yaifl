{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Model.Properties.Openable
  ( -- * Types
    Openable(..)
  , getOpenable
  ) where


import Solitude

import Yaifl.Model.Objects.Query
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH
import Yaifl.Model.Objects.Effects

-- | Whether the thing is open or not.
data Openable = Open | Closed
  deriving stock (Eq, Show, Read, Ord, Generic)

makeSpecificsWithout [] ''Openable
