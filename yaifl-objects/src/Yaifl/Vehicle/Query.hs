module Yaifl.Vehicle.Query
  ( setVehicle
  , modifyVehicle
  , thingIsOpenVehicle
  , thingIsClosedVehicle
  , isVehicle
  , getVehicle
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.AnyObject
import Yaifl.Object.Kind
import Yaifl.Thing.Kind
import Yaifl.Metadata
import Yaifl.ObjectLike
import Yaifl.Property.Query( defaultPropertySetter, modifyProperty )
import Yaifl.TH ( WMWithProperty, makeModify )
import Yaifl.Openable.Kind
import Yaifl.Vehicle.Kind

makeModify ''Vehicle

-- | Check if @o@ is of the @Vehicle@ type.
isVehicle ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isVehicle o = getObject o >>= (`isKind` "vehicle")

thingIsOpenVehicle ::
  WMWithProperty wm Vehicle
  => Thing wm
  -> Bool
thingIsOpenVehicle = (== Just Open) . fmap (view (#container % #openable % #opened)) . getVehicleMaybe

thingIsClosedVehicle ::
  WMWithProperty wm Vehicle
  => Thing wm
  -> Bool
thingIsClosedVehicle = (== Just Closed) . fmap (view (#container % #openable % #opened)) . getVehicleMaybe

getVehicle ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Vehicle
  => VehicleEntity
  -> Eff es Vehicle
getVehicle de = do
  t <- getThing de
  return $ fromMaybe (error "property witness violated") $ getVehicleMaybe t
