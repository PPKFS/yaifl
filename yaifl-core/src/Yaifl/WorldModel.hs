{-|
Module      : Yaifl.WorldModel
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

The WorldModel system provides a type-level configuration mechanism that
bundles all of Yaifl's type parameters into a single manageable type.
This avoids the "parameter explosion" problem that would otherwise require
passing many individual type parameters throughout the codebase.

This module defines:

- `WorldModel`: Core configuration type containing all type parameters
- Type families for accessing components (WMObjSpecifics, WMDirection, WMValues, etc.)
- Constraint synonyms for common typeclass requirements (WMShow, WMRead, etc.)

The WorldModel enables:
- Centralized type configuration for games
- Clean separation between type parameters and implementation
- Extensible architecture through type families
- Type-safe access to game components
-}

module Yaifl.WorldModel (
  -- * World model
    WorldModel(..)
  -- ** Type families
  , WMObjSpecifics
  , WMValues
  , WMDirection
  , WMThingData
  , WMRoomData
  , WMRegionData
  , WMActivities
  , WMResponses
  , WMText
  , WMActions
  -- ** Constraints
  , WMShow
  , WMRead
  , WMOrd
  , WMEq

  ) where

import Yaifl.Prelude

-- | All the various type parameters wrapped into a single type.
-- This allows us to tie the knot in some weird way - we need some way to refer
-- to sets of activities or actions or object types, but these then need to depend
-- on the fundamental types themselves.
-- | The core configuration type that bundles all of Yaifl's type parameters.
--
-- This data type serves as a central configuration point that avoids parameter explosion
-- by bundling all type parameters into a single manageable type. Each field represents
-- a different aspect of the game's type configuration:
--
-- - `wmObjSpecifics`: Type for object-specific data (see `WMObjSpecifics`)
-- - `wmDirection`: Type for directional values (see `WMDirection`)
-- - `wmValues`: Type for global game state values (see `WMValues`)
-- - `wmThingData`: Additional fields for thing objects (extensions to standard `Yaifl.Thing.Kind`)
-- - `wmRoomData`: Additional fields for room objects (extensions to standard `Yaifl.Room.Kind`)
-- - `wmRegionData`: Additional fields for region objects (extensions to standard `Yaifl.Region.Kind`)
-- - `wmActivities`: Activity record type (see `WMActivities`)
-- - `wmResponses`: Response record type (see `WMResponses`)
-- - `wmText`: Dynamic text type (see `WMText`)
-- - `wmActions`: Action record type (see `WMActions`)
--
-- These extension fields allow game authors to add custom properties to core types without
-- modifying the standard definitions. If no additional properties are needed, use `()`.
--
-- Example usage:
-- @
--   -- Minimal world model using standard types with no extensions
--   type SimpleWorldModel = 'WorldModel' 
--     'Yaifl.Model.ObjectSpecifics'  -- Standard object specifics
--     'Yaifl.Direction.Kind'        -- Standard directions
--     MyGameState                   -- Custom game state
--     () () ()                      -- No extensions to Things, Rooms, Regions
--     'Yaifl.Activity.Record'       -- Standard activities
--     'Yaifl.Text.Response.Record'  -- Standard responses
--     'Yaifl.Text.DynamicText'      -- Standard dynamic text
--     'Yaifl.Action.Record'         -- Standard actions
-- @
data WorldModel =
  WorldModel
    { wmObjSpecifics :: Type
    , wmDirection :: Type
    , wmValues :: Type
    , wmThingData :: Type
    , wmRoomData :: Type
    , wmRegionData :: Type
    , wmActivities :: WorldModel -> Type
    , wmResponses :: WorldModel -> Type
    , wmText :: WorldModel -> Type
    , wmActions :: WorldModel -> Type
    }

-- | Object specifics (see `Yaifl.Model.ObjectSpecifics` for the canonical instantiation).
type family WMObjSpecifics (wm :: WorldModel) :: Type where
  WMObjSpecifics ('WorldModel objSpec dir v td rd red a r re ac) = objSpec

-- | Directions, which is required to be a superset of `Yaifl.Direction.Kind`.
type family WMDirection (wm :: WorldModel) :: Type where
  WMDirection ('WorldModel objSpec dir v td rd red a r re ac) = dir

-- | Record of values passed around as a global state.
type family WMValues (wm :: WorldModel) :: Type where
  WMValues ('WorldModel objSpec dir v td rd red a r re ac) = v

-- | Additional fields carried by `Yaifl.Core.ThingData`; i.e. data extensionality for all `Yaifl.Core.Thing`s.
type family WMThingData (wm :: WorldModel) :: Type where
  WMThingData ('WorldModel objSpec dir v td rd red a r re ac) = td

-- | Additional fields carried by `Yaifl.Core.RoomData`; i.e. data extensionality for all `Yaifl.Core.Room`s.
type family WMRoomData (wm :: WorldModel) :: Type where
  WMRoomData ('WorldModel objSpec dir v td rd red a r re ac) = rd

-- | Unused currently.
type family WMRegionData (wm :: WorldModel) :: Type where
  WMRegionData ('WorldModel objSpec dir v td rd red a r re ac) = red

-- | The record of all `Yaifl.Activity`.
type family WMActivities (wm :: WorldModel) :: Type where
  WMActivities ('WorldModel objSpec dir v td rd red a r re ac) = a ('WorldModel objSpec dir v td rd red a r re ac)

-- | The record of all `Yaifl.Text.Response`s.
type family WMResponses (wm :: WorldModel) :: Type where
  WMResponses ('WorldModel objSpec dir v td rd red a r re ac) = r ('WorldModel objSpec dir v td rd red a r re ac)

-- | To avoid circular dependencies, the dynamic text type (a fancy forall es. Eff es Text made
-- with quasiquoters).
type family WMText (wm :: WorldModel) :: Type where
  WMText ('WorldModel objSpec dir v td rd red a r re ac) = re ('WorldModel objSpec dir v td rd red a r re ac)

-- | Currently unused.
type family WMActions (wm :: WorldModel) :: Type where
  WMActions ('WorldModel objSpec dir v td rd red a r re ac) = ac ('WorldModel objSpec dir v td rd red a r re ac)

type WMConstr (c :: Type -> Constraint) wm = (c (WMObjSpecifics wm), c (WMValues wm), c (WMDirection wm))

-- | Constraint that object specifics, values, and directions are Showable.
type WMShow wm = WMConstr Show wm
-- | Constraint that object specifics, values, and directions are Readble.
type WMRead wm = WMConstr Read wm
-- | Constraint that object specifics, values, and directions are Ordable.
type WMOrd wm = WMConstr Ord wm
-- | Constraint that object specifics, values, and directions are Eqable.
type WMEq wm = WMConstr Eq wm