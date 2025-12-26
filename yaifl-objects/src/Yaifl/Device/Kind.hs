module Yaifl.Device.Kind
  ( Device(..)
  , getDeviceMaybe
  ) where

import Yaifl.Prelude

import Yaifl.AnyObject
import Yaifl.Property.Query
import Yaifl.TH

newtype Device = Device
  { switchedOn :: Bool
  } deriving newtype (Show)
    deriving stock (Eq, Ord, Generic, Read)

instance Pointed Device where
  identityElement = Device False

makeFieldLabelsNoPrefix ''Device
makeGetMaybe ''Device
