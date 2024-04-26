module Yaifl.Gen.City.Building
( Building(..)
, BuildingFloor(..)
, ApartmentTowerBase(..)
) where

import Yaifl.Prelude
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Region
import Yaifl.Model.WorldModel

data Building wm = Building
  { name :: Text
  , floors :: NonEmpty (BuildingFloor wm)
  , buildingBase :: ApartmentTowerBase wm
  }

data BuildingFloor wm = BuildingFloor
  { level :: Int
  , entrances :: Int
  , exits :: (RoomEntity, WMDirection wm)
  , floorRooms :: [RoomEntity]
  , floorRegion :: RegionEntity
  } deriving stock (Generic)
deriving stock instance Show (WMDirection wm) => Show (Building wm)
deriving stock instance Show (WMDirection wm) => Show (BuildingFloor wm)


data ApartmentTowerBase wm = ApartmentTowerBase
  { name :: Text
  , region :: RegionEntity
  , numberOfFloors :: Int
  , entranceIsOnFace :: WMDirection wm
  } deriving stock (Generic)

deriving stock instance Show (WMDirection wm) => Show (ApartmentTowerBase wm)
deriving stock instance Eq (WMDirection wm) => Eq (ApartmentTowerBase wm)

makeFieldLabelsNoPrefix ''ApartmentTowerBase
makeFieldLabelsNoPrefix ''BuildingFloor