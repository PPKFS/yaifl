{-|
Module      : Yaifl.Device.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Device components represent objects with on/off states,
providing basic functionality for switchable objects.

This module defines the `Device` type and its associated components:

- `Device`: Simple on/off state component
- Functions for creating and querying device states
-}

module Yaifl.Device.Kind
  ( -- * Device types
    Device(..)

    -- * Device functions
  , getDeviceMaybe
  ) where

import Yaifl.Prelude

import Yaifl.AnyObject
import Yaifl.Property.Query
import Yaifl.TH

-- | A simple on/off state component for devices.
-- The `switchedOn` field indicates whether the device is currently active.
newtype Device = Device
  { switchedOn :: Bool
  } deriving newtype (Show)
    deriving stock (Eq, Ord, Generic, Read)

instance Pointed Device where
  identityElement = Device False

makeFieldLabelsNoPrefix ''Device
makeGetMaybe ''Device
