{-|
Module      : Yaifl.Region.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Regions represent spatial organisations in the game world that group
related rooms and provide hierarchical structure.
-}

module Yaifl.Region.Kind (
  -- * Region types
    RegionTag
  , RegionEntity
  , Region(..)
  ) where

import Yaifl.Prelude
import Yaifl.Entity (TaggedEntity, RoomEntity)
import qualified Data.Set as S
import Yaifl.WorldModel
import Yaifl.Object.Kind

data RegionTag
type RegionEntity = TaggedEntity RegionTag

-- | Spatial organization that groups related rooms.
-- Example hierarchy:
-- @
-- World
-- ├── Continent
-- │   ├── Country
-- │   │   ├── City (with rooms)
-- │   │   └── Wilderness (with rooms)
-- └── Underworld (with rooms)
-- @
data Region wm = Region
  { regionID :: RegionEntity
    -- ^ Unique identifier for this region. Used for entity reference and equality testing.
  , name :: Text
    -- ^ Display name of the region. This is what players will see when the region is referenced.
  , namePrivacy :: NamePrivacy
    -- ^ Visibility settings for the region name. Controls when and how the name is displayed to players.
  , subRegions :: S.Set RegionEntity
    -- ^ Child regions contained within this region. Forms the hierarchical structure.
  , superRegion :: Maybe RegionEntity
    -- ^ Parent region, if this region is nested within a larger region. 'Nothing' for top-level regions.
  , rooms :: S.Set RoomEntity
    -- ^ Rooms that belong to this region. Rooms can only belong to 0-1 regions at a time.
  , regionData :: WMRegionData wm
    -- ^ World-model-specific region data. Allows different world models to extend regions with custom data.
  } deriving stock (Generic)

deriving stock instance Show (WMRegionData wm) => Show (Region wm)

makeFieldLabelsNoPrefix ''Region
