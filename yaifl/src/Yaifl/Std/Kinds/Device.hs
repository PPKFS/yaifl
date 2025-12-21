module Yaifl.Std.Kinds.Device
  ( Device(..)
  , switchItOn
  , switchItOff
  , toggleDevice
  , getDeviceMaybe
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.AnyObject
import Yaifl.Thing.Kind
import Yaifl.Property.Query
import Yaifl.TH

newtype Device = Device
  { switchedOn :: Bool
  } deriving newtype (Show)
    deriving stock (Eq, Ord, Generic, Read)

instance Pointed Device where
  identityElement = Device False

makeFieldLabelsNoPrefix ''Device
makeSpecificsWithout [] ''Device

switchItOn ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Device
  => Thing wm
  -> Eff es ()
switchItOn = flip modifyDevice (#switchedOn .~ True)

switchItOff ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Device
  => Thing wm
  -> Eff es ()
switchItOff = flip modifyDevice (#switchedOn .~ False)

toggleDevice ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Device
  => Thing wm
  -> Eff es ()
toggleDevice = flip modifyDevice (#switchedOn %~ not)