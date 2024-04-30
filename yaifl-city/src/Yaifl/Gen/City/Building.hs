module Yaifl.Gen.City.Building
( Building(..)
, BuildingFloor(..)
, ApartmentTowerBase(..)
, BuildingGeneration
) where

import Yaifl.Prelude
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Region
import Yaifl.Model.WorldModel
import Yaifl.Model.Kinds
import Yaifl.Model.Effects
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.HasProperty
import Yaifl.Game.Create
import Yaifl.Model.MultiLocated
import Yaifl.Model.Kinds.Enclosing

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

type BuildingGeneration wm es =
  ( Pointed (WMRegionData wm)
  , ObjectUpdate wm :> es
  , RuleEffects wm es
  , NoMissingObjects wm es
  , WMStdDirections wm
  , WMWithProperty wm MultiLocated
  , WMWithProperty wm Enclosing
  , WMHasObjSpecifics wm
  , AddObjects wm es
  , Semigroup (WMSayable wm)
  )