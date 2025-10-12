module Yaifl.Std.Kinds.Device
  ( Device(..)
  , switchItOn
  , switchItOff
  , toggleDevice
  , getDeviceMaybe
  ) where

import Yaifl.Prelude

import Yaifl.Core.Effects
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Query.Property
import Yaifl.Core.TH

newtype Device = Device
  { switchedOn :: Bool
  } deriving newtype (Show)
    deriving stock (Eq, Ord, Generic, Read)

instance Pointed Device where
  identityElement = Device False

makeFieldLabelsNoPrefix ''Device
makeSpecificsWithout [] ''Device

switchItOn ::
  NoMissingObjects wm es
  => WMWithProperty wm Device
  => Thing wm
  -> Eff es ()
switchItOn = flip modifyDevice (#switchedOn .~ True)

switchItOff ::
  NoMissingObjects wm es
  => WMWithProperty wm Device
  => Thing wm
  -> Eff es ()
switchItOff = flip modifyDevice (#switchedOn .~ False)

toggleDevice ::
  NoMissingObjects wm es
  => WMWithProperty wm Device
  => Thing wm
  -> Eff es ()
toggleDevice = flip modifyDevice (#switchedOn %~ not)