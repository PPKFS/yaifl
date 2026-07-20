{-|
Module      : Yaifl.WorldModel
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Type-level configuration mechanism bundling Yaifl's type parameters.

Provides:
- `WorldModel`: Core configuration type
- Type families for component access
- Constraint synonyms for common requirements

Enables centralized configuration, extensibility, and type-safe component access.
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

-- | The core configuration type that bundles all of Yaifl's type parameters.
--
-- This allows centralized type configuration and avoids parameter explosion.
-- The type families enable type-safe access to game components while maintaining
-- extensibility through the dependent types.
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

-- | Direction types for spatial relationships.
type family WMDirection (wm :: WorldModel) :: Type where
  WMDirection ('WorldModel objSpec dir v td rd red a r re ac) = dir

-- | Global state values.
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

-- | Helper constraint for applying a constraint to core WorldModel components.
--
-- Applies constraint `c` to object specifics, values, and directions.
-- Used to define the standard constraint synonyms below.
type WMConstr (c :: Type -> Constraint) wm = (c (WMObjSpecifics wm), c (WMValues wm), c (WMDirection wm))


type WMShow wm = WMConstr Show wm

type WMRead wm = WMConstr Read wm

type WMOrd wm = WMConstr Ord wm

type WMEq wm = WMConstr Eq wm