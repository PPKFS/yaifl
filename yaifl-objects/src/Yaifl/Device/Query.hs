module Yaifl.Device.Query
  ( switchItOn
  , switchItOff
  , toggleDevice
  , setDevice
  , modifyDevice
  ) where

import Yaifl.Prelude
import Yaifl.Effects.ObjectQuery
import Yaifl.Thing.Kind
import Yaifl.Device.Kind
import Yaifl.AnyObject
import Yaifl.Property.Query
import Yaifl.TH

makeModify ''Device

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