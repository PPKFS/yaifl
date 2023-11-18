{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Model.Properties.Openable
  ( -- * Types
    Openable(..)
  , getOpenableMaybe
  ) where



import Solitude

import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Object

-- | Whether the thing is open or not.
data Openable = Open | Closed
  deriving stock (Eq, Show, Read, Ord, Generic)

makeSpecificsWithout [] ''Openable