module Yaifl.Core.Query.Region
  ( areInRegion
  , isInRegion
  , isSubregionOf
  , modifyRegion
  , roomsInRegion
  ) where

import Yaifl.Prelude

import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Model.Kinds.Region
import qualified Data.Set as S


areInRegion ::
  NoMissingObjects wm es
  => Foldable f
  => f RoomEntity
  -> RegionEntity
  -> Eff es ()
areInRegion f r = mapM_ (`isInRegion` r) f

isInRegion ::
  NoMissingObjects wm es
  => RoomEntity
  -> RegionEntity
  -> Eff es ()
isInRegion r reg = do
  modifyRegion reg (#rooms %~ S.insert r)

modifyRegion ::
  NoMissingObjects wm es
  => RegionEntity
  -> (Region wm -> Region wm)
  -> Eff es ()
modifyRegion o u = do
  r <- lookupRegion o
  whenRight_ r $ \r' -> setRegion (u r')

isSubregionOf ::
  NoMissingObjects wm es
  => RegionEntity
  -> RegionEntity
  -> Eff es ()
isSubregionOf subReg reg = do
  modifyRegion reg (#subRegions %~ S.insert subReg)

roomsInRegion ::
  NoMissingObjects wm es
  => Region wm
  -> Eff es (S.Set RoomEntity)
roomsInRegion r = do
  subRegs <- rights <$> mapM lookupRegion (S.toList $ subRegions r)
  rs <- mapM roomsInRegion subRegs
  pure $ S.unions $ rooms r : rs