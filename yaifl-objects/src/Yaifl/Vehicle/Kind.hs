module Yaifl.Vehicle.Kind
  ( -- * Types
  Vehicle(..)
  , VehicleEntity
  , VehicleTag
  , inThe
  , getVehicleMaybe
  , TaggedVehicle
  ) where


import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.AnyObject
import Yaifl.Enclosing.Kind
import Yaifl.Thing.Kind
import Yaifl.Enclosing.Query
import Yaifl.Property.Query( defaultPropertyGetter )
import Yaifl.TH ( WMWithProperty, makeGetMaybe )
import Yaifl.ObjectLike
import Yaifl.Container.Kind hiding (inThe)

newtype Vehicle = Vehicle
  { container :: Container
  } deriving stock (Show, Eq, Generic)

data VehicleTag
type VehicleEntity = TaggedEntity VehicleTag

type TaggedVehicle wm = TaggedObject (Thing wm) VehicleTag

inThe ::
  VehicleEntity
  -> EnclosingEntity
inThe = coerceTag

instance Taggable VehicleEntity EnclosingTag
instance Taggable Vehicle EnclosingTag
instance Taggable Vehicle VehicleTag
instance Taggable Vehicle ThingTag
instance Taggable VehicleEntity ThingTag

instance IsEnclosing VehicleEntity

makeFieldLabelsNoPrefix ''Vehicle

makeGetMaybe ''Vehicle

instance ThingLike wm VehicleEntity where
  getThing = getThing . coerceTag @ThingTag
