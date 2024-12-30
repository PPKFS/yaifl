module Yaifl.Model.Kinds.Region where

import Yaifl.Prelude
import Yaifl.Core.Entity (TaggedEntity, RoomEntity)
import qualified Data.Set as S
import Yaifl.Model.WorldModel
import Yaifl.Core.Kinds.Object

data RegionTag
type RegionEntity = TaggedEntity RegionTag

data Region wm = Region
  { regionID :: RegionEntity
  , name :: Text
  , namePrivacy :: NamePrivacy
  , subRegions :: S.Set RegionEntity
  , superRegion :: Maybe RegionEntity
  , rooms :: S.Set RoomEntity
  , regionData :: WMRegionData wm
  } deriving stock (Generic)

deriving stock instance Show (WMRegionData wm) => Show (Region wm)

makeFieldLabelsNoPrefix ''Region
