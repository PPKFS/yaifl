{-|
Module      : Yaifl.Region.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Regions represent spatial organisations in the game world that group
related rooms and provide hierarchical structure.

This module defines the `Region` type and its associated components:

- `Region`: The core region type with spatial organisation data
- `RegionTag`: Phantom type for region entity tagging
- `RegionEntity`: Type-safe reference to region objects
- Functions for creating and manipulating regions
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

-- | A spatial organisation that groups related rooms.
-- Regions form hierarchical structures with parent-child relationships,
-- enabling logical organisation of game worlds.
data Region wm = Region
  { regionID :: RegionEntity
    -- ^ Unique identifier for this region
  , name :: Text
    -- ^ Display name of the region
  , namePrivacy :: NamePrivacy
    -- ^ Visibility of the region name
  , subRegions :: S.Set RegionEntity
    -- ^ Child regions contained within this region
  , superRegion :: Maybe RegionEntity
    -- ^ Parent region, if this region is nested
  , rooms :: S.Set RoomEntity
    -- ^ Rooms that belong to this region
  , regionData :: WMRegionData wm
    -- ^ World-model-specific region data
  } deriving stock (Generic)

deriving stock instance Show (WMRegionData wm) => Show (Region wm)

makeFieldLabelsNoPrefix ''Region
