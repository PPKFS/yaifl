module Yaifl.Model.Kinds.Device
  ( Device(..)
  , switchItOn
  , switchItOff
  , toggleDevice

  ) where

import Yaifl.Prelude

import Yaifl.Model.Effects
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.AnyObject
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Query
import Yaifl.Model.TH (makeSpecificsWithout)

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