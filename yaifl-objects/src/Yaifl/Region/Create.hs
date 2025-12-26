module Yaifl.Region.Create
  (

  ) where

import Yaifl.Prelude

import Yaifl.Object.Kind
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.WorldModel

import qualified Data.Set as S
import Yaifl.Region.Kind (RegionEntity, Region (..))

addRegion ::
  Pointed (WMRegionData wm)
  => ObjectQuery wm :> es
  => Text
  -> Eff es RegionEntity
addRegion n = do
  rId <- generateEntity False
  let r = Region (unsafeTagEntity rId) n PubliclyNamed S.empty Nothing S.empty identityElement
  setRegion r
  pure (unsafeTagEntity rId)
