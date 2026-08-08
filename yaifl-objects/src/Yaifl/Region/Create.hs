module Yaifl.Region.Create
  ( addRegion

  ) where

import Yaifl.Prelude

import Yaifl.Object.Kind
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.WorldModel


import Yaifl.Region.Kind (RegionEntity(..), Region (..))
import qualified Data.Set as S

addRegion ::
  Pointed (WMRegionData wm)
  => ObjectQuery wm :> es
  => Text
  -> Eff es RegionEntity
addRegion n = do
  rId <- generateEntity False
  let r = Region
        { regionID = (RegionEntity rId)
        , name = n
        , namePrivacy = PubliclyNamed
        , subRegions = S.empty
        , superRegion = Nothing
        , rooms = S.empty
        , regionData = identityElement
        , backdrops = S.empty }
  setRegion r
  pure (RegionEntity rId)
