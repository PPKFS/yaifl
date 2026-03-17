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

Regions serve several important purposes in game world organisation:

1. **Hierarchical spatial structure**: Regions can contain sub-regions, creating
   a tree-like organisation of the game world (e.g., continent → country → city → district)

2. **Logical grouping**: Rooms with similar themes or purposes can be grouped together
   (e.g., "castle grounds", "dungeon level 1", "enchanted forest")

3. **Navigation aids**: Regions help players understand the spatial relationships
   between different areas of the game world

4. **World management**: Regions provide a way to organise and manage large game
   worlds efficiently

Example usage:

@
-- Create a simple region
castleRegion <- createRegion "Castle Grounds"
  { subRegions = S.empty
  , superRegion = Nothing
  , rooms = S.singleton greatHallRoom
  }

-- Create a nested region hierarchy
forestRegion <- createRegion "Enchanted Forest"
  { subRegions = S.singleton darkGroveRegion
  , superRegion = Just wildernessRegion
  , rooms = S.fromList [clearingRoom, streamRoom]
  }
@
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
--
-- Regions form hierarchical structures with parent-child relationships,
-- enabling logical organisation of game worlds. Each region can contain
-- multiple rooms and sub-regions, creating a tree-like structure that
-- represents the spatial organisation of the game environment.
--
-- The 'Region' type is parameterised by 'wm' (world model) to allow for
-- world-model-specific data storage while maintaining a consistent interface.
--
-- Example of region hierarchy:
--
-- @
-- World
-- ├── Continent
-- │   ├── Country
-- │   │   ├── City
-- │   │   │   ├── District 1
-- │   │   │   └── District 2
-- │   │   └── Wilderness
-- │   └── Ocean
-- └── Underworld
-- @
--
-- Performance note: The use of 'S.Set' for storing sub-regions and rooms
-- provides efficient lookup and membership testing.

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
    -- ^ Rooms that belong to this region. Rooms can only belong to one region at a time.
  , regionData :: WMRegionData wm
    -- ^ World-model-specific region data. Allows different world models to extend regions with custom data.
  } deriving stock (Generic)

deriving stock instance Show (WMRegionData wm) => Show (Region wm)

makeFieldLabelsNoPrefix ''Region
