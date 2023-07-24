{-|
Module      : Yaifl.Model.WorldModel
Description : Some type family nonsense to avoid writing massive amounts of type parameters.
Copyright   : (c) Avery 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
-}

module Yaifl.Model.WorldModel (
  -- * World model
    WorldModel(..)
  , WMObjSpecifics
  , WMValues
  , WMDirection
  , WMRulebooks
  , WMActivities
  , WMResponses
  , WMSayable
  , WMShow
  , WMRead
  , WMOrd
  , WMEq
  ) where

import Solitude

-- | All the various type parameters wrapped into a single type.
data WorldModel = WorldModel Type Type Type Type (WorldModel -> Type) (WorldModel -> Type) (WorldModel -> Type)

type family WMObjSpecifics (wm :: WorldModel) :: Type where
  WMObjSpecifics ('WorldModel objSpec dir o v a r re) = objSpec

type family WMDirection (wm :: WorldModel) :: Type where
  WMDirection ('WorldModel objSpec dir o v a r re) = dir

type family WMValues (wm :: WorldModel) :: Type where
  WMValues ('WorldModel objSpec dir o v a r re) = o

type family WMRulebooks (wm :: WorldModel) :: Type where
  WMRulebooks ('WorldModel objSpec dir o v a r re) = v

type family WMActivities (wm :: WorldModel) :: Type where
  WMActivities ('WorldModel objSpec dir o v a r re) = a ('WorldModel objSpec dir o v a r re)

type family WMResponses (wm :: WorldModel) :: Type where
  WMResponses ('WorldModel objSpec dir o v a r re) = r ('WorldModel objSpec dir o v a r re)

type family WMSayable (wm :: WorldModel) :: Type where
  WMSayable ('WorldModel objSpec dir o v a r re) = re ('WorldModel objSpec dir o v a r re)

type WMConstr (c :: Type -> Constraint) wm = (c (WMObjSpecifics wm), c (WMValues wm), c (WMDirection wm))
type WMShow wm = WMConstr Show wm
type WMRead wm = WMConstr Read wm
type WMOrd wm = WMConstr Ord wm
type WMEq wm = WMConstr Eq wm