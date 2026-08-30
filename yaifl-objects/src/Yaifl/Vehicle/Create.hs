{-# LANGUAGE RecordWildCards #-}
module Yaifl.Vehicle.Create
  ( addVehicle
  , VehicleConfig(..)
  , newVehicle

  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.Object.Create
import Yaifl.Thing.Kind
import Yaifl.Vehicle.Kind
import Yaifl.Openable.Kind
import Yaifl.Tag
import Yaifl.WorldModel
import Yaifl.ObjectSpecifics
import Yaifl.Thing.Create
import Yaifl.Container.Kind

data VehicleConfig wm = VehicleConfig
  { description :: WMText wm
  , initialAppearance :: WMText wm
  , location :: Maybe EnclosingEntity
  , thingModify :: Eff '[State (Thing wm)] ()
  , opacity :: Opacity
  , openStatus :: (Opened, Openable)
  , carryingCapacity :: Int
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''VehicleConfig

newVehicle :: IsString (WMText wm) => VehicleConfig wm
newVehicle = VehicleConfig
  { description = ""
  , initialAppearance = ""
  , thingModify = pass
  , opacity = Opaque
  , carryingCapacity = 100
  , openStatus = (Open, NotOpenable)
  , location = Nothing
  }

addVehicle ::
  forall wm es.
  AddObjects wm es
  => WMText wm
  -> VehicleConfig wm
  -> Eff es VehicleEntity
addVehicle name VehicleConfig{..} = do
    let cs = Vehicle $ makeContainer (Just carryingCapacity) (Just opacity) (Just Enterable) (Just $ snd openStatus) (Just $ fst openStatus)
    c <- addThing name newThing
          { description
          , initialAppearance
          , specifics = inj (Proxy @wm) $ VehicleSpecifics cs
          , location
          , thingModify
          }
    pure $ tagEntity @Vehicle @VehicleTag cs c