{-|
Module      : Yaifl.Core.WorldModel
Description : Some type family nonsense to avoid writing massive amounts of type parameters.
Copyright   : (c) Avery 2022-2024
License     : MIT
Maintainer  : ppkfs@outlook.com

To save having to write increasingly large amounts of type parameters.
-}

module Yaifl.Core.WorldModel (
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

-- | Directions, which is required to be a superset of `Yaifl.Std.Kinds.Direction`.
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