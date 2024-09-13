{-|
Module      : Yaifl.Model.WorldModel
Description : Some type family nonsense to avoid writing massive amounts of type parameters.
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

To save having to write increasingly large amounts of type parameters.
-}

module Yaifl.Model.WorldModel (
  -- * World model
    WorldModel(..)
  -- ** Type families
  , WMObjSpecifics
  , WMValues
  , WMDirection
  , WMRegionData
  , WMActivities
  , WMResponses
  , WMText
  -- ** Constraints
  , WMShow
  , WMRead
  , WMOrd
  , WMEq
  ) where

import Yaifl.Prelude

-- | All the various type parameters wrapped into a single type.
data WorldModel =
  WorldModel
    Type
    Type
    Type
    Type
    (WorldModel -> Type)
    (WorldModel -> Type)
    (WorldModel -> Type)

-- | Object specifics (see `Yaifl.Model.ObjectSpecifics` for the canonical instantiation).
type family WMObjSpecifics (wm :: WorldModel) :: Type where
  WMObjSpecifics ('WorldModel objSpec dir o v a r re) = objSpec

-- | Directions, which is required to be a superset of `Yaifl.Model.Kinds.Direction`.
type family WMDirection (wm :: WorldModel) :: Type where
  WMDirection ('WorldModel objSpec dir o v a r re) = dir

-- | Record of values passed around as a global state.
type family WMValues (wm :: WorldModel) :: Type where
  WMValues ('WorldModel objSpec dir o v a r re) = o

-- | Unused currently.
type family WMRegionData (wm :: WorldModel) :: Type where
  WMRegionData ('WorldModel objSpec dir o v a r re) = v

-- | The record of all `Yaifl.Activity`.
type family WMActivities (wm :: WorldModel) :: Type where
  WMActivities ('WorldModel objSpec dir o v a r re) = a ('WorldModel objSpec dir o v a r re)

-- | The record of all `Yaifl.Text.Response`s.
type family WMResponses (wm :: WorldModel) :: Type where
  WMResponses ('WorldModel objSpec dir o v a r re) = r ('WorldModel objSpec dir o v a r re)

-- | To avoid circular dependencies, the dynamic text type (a fancy forall es. Eff es Text made
-- with quasiquoters).
type family WMText (wm :: WorldModel) :: Type where
  WMText ('WorldModel objSpec dir o v a r re) = re ('WorldModel objSpec dir o v a r re)

type WMConstr (c :: Type -> Constraint) wm = (c (WMObjSpecifics wm), c (WMValues wm), c (WMDirection wm))

-- | Constraint that object specifics, values, and directions are Showable.
type WMShow wm = WMConstr Show wm
-- | Constraint that object specifics, values, and directions are Readble.
type WMRead wm = WMConstr Read wm
-- | Constraint that object specifics, values, and directions are Ordable.
type WMOrd wm = WMConstr Ord wm
-- | Constraint that object specifics, values, and directions are Eqable.
type WMEq wm = WMConstr Eq wm