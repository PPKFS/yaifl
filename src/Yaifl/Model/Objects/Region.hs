module Yaifl.Model.Objects.Region where

import Solitude
import Yaifl.Model.Objects.Entity (RoomEntity, TaggedEntity)
import qualified Data.Set as S
import Yaifl.Model.WorldModel

data RegionTag
type RegionEntity = TaggedEntity RegionTag

data Region wm = Region
  { regionID :: RegionEntity
  , name :: Text
  , subRegions :: S.Set RegionEntity
  , superRegion :: Maybe RegionEntity
  , rooms :: S.Set RoomEntity
  , regionData :: WMRegionData wm
  } deriving stock (Generic)

deriving stock instance Show (WMRegionData wm) => Show (Region wm)

makeFieldLabelsNoPrefix ''Region
