{-|
Module      : Yaifl.Core.WorldModel
Description : Some type family nonsense to avoid writing massive amounts of type parameters.
Copyright   : (c) Avery 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
-}

module Yaifl.Core.WorldModel (
  -- * World model
    WorldModel(..)
  , WMObjSpecifics
  , WMValues
  , WMDirection
  , WMShow
  , WMRead
  , WMOrd
  , WMEq
  ) where

import Solitude

-- | All the various type parameters wrapped into a single type.
data WorldModel = WorldModel Type Type Type Type

type family WMObjSpecifics (wm :: WorldModel) :: Type where
  WMObjSpecifics ('WorldModel objSpec dir o v) = objSpec

type family WMDirection (wm :: WorldModel) :: Type where
  WMDirection ('WorldModel objSpec dir o v) = dir

type family WMValues (wm :: WorldModel) :: Type where
  WMValues ('WorldModel objSpec dir o v) = o

type WMConstr (c :: Type -> Constraint) wm = (c (WMObjSpecifics wm), c (WMValues wm), c (WMDirection wm))
type WMShow wm = WMConstr Show wm
type WMRead wm = WMConstr Read wm
type WMOrd wm = WMConstr Ord wm
type WMEq wm = WMConstr Eq wm