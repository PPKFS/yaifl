{-# LANGUAGE RecordWildCards #-}
module Yaifl.Device.Create
  ( addDevice
  , DeviceConfig(..)
  , newDevice
  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Object.Create
import Yaifl.Device.Kind
import Yaifl.WorldModel
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create
import Yaifl.Door.Create
import Yaifl.Text.DynamicText

data DeviceConfig wm p = DeviceConfig
  { description :: WMText wm
  , switchedOn :: Bool
  , initialAppearance :: WMText wm
  } deriving stock (Generic)

newDevice :: (WMText wm ~ DynamicText wm) => DeviceConfig wm 'Complete
newDevice = DeviceConfig
  { description = ""
  , switchedOn = False
  , initialAppearance = ""
  }

addDevice ::
  forall wm es.
  AddObjects wm es
  => WMText wm
  -> DeviceConfig wm 'Complete
  -> Eff es ThingEntity
addDevice name DeviceConfig{..} = addThing name newThing
  { initialAppearance
  , description
  , specifics = inj (Proxy @wm) (DeviceSpecifics (Device switchedOn))
  , objType = ObjectKind "device"
  }