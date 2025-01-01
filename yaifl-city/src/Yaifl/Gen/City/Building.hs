module Yaifl.Gen.City.Building
( Building(..)
, BuildingFloor(..)
, ApartmentTowerBase(..)
, BuildingGeneration
) where

import Yaifl.Prelude
import Yaifl.Core.Entity
import Yaifl.Std.Kinds.Region
import Yaifl.Core.WorldModel
import Yaifl.Core.Effects
import Yaifl.Core.Rules.Rulebook
import Yaifl.Core.HasProperty
import Yaifl.Std.Create
import Yaifl.Std.Kinds.MultiLocated
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Std.Kinds.Door
import Yaifl.Text.Say (WithPrintingNameOfSomething)
import Yaifl.Std.Activities.ListingContents (WithListingContents)

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
  , WithPrintingNameOfSomething wm
  , WithListingContents wm
  , NoMissingObjects wm es
  , WMStdDirections wm
  , WMWithProperty wm MultiLocated
  , WMWithProperty wm Enclosing
  , WMWithProperty wm Door
  , WMHasObjSpecifics wm
  , AddObjects wm es
  , Semigroup (WMText wm)
  )