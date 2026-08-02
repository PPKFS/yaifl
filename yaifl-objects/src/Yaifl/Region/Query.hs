module Yaifl.Region.Query
  ( areInRegion
  , isInRegion
  , isSubregionOf
  , modifyRegion
  , roomsInRegion
  , getRegion
  , getEnclosingRegions
  , getRegionHierarchy
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Region.Kind
import qualified Data.Set as S
import Yaifl.Room.Kind
import Yaifl.Object.Kind

getRegion ::
  WithoutMissingObjects wm es
  => RegionEntity
  -> Eff es (Region wm)
getRegion r = lookupRegion r >>= return . fromRight (error $ "failed to find region " <> show r)

areInRegion ::
  WithoutMissingObjects wm es
  => Foldable f
  => f RoomEntity
  -> RegionEntity
  -> Eff es ()
areInRegion f r = mapM_ (`isInRegion` r) f

isInRegion ::
  WithoutMissingObjects wm es
  => RoomEntity
  -> RegionEntity
  -> Eff es ()
isInRegion r reg = do
  modifyRegion reg (#rooms %~ S.insert r)

modifyRegion ::
  WithoutMissingObjects wm es
  => RegionEntity
  -> (Region wm -> Region wm)
  -> Eff es ()
modifyRegion o u = do
  r' <- getRegion o
  setRegion (u r')

isSubregionOf ::
  WithoutMissingObjects wm es
  => RegionEntity
  -> RegionEntity
  -> Eff es ()
isSubregionOf subReg reg = do
  modifyRegion reg (#subRegions %~ S.insert subReg)

roomsInRegion ::
  WithoutMissingObjects wm es
  => Region wm
  -> Eff es (S.Set RoomEntity)
roomsInRegion r = do
  subRegs <- rights <$> mapM lookupRegion (S.toList $ subRegions r)
  rs <- mapM roomsInRegion subRegs
  pure $ S.unions $ rooms r : rs

getRegionHierarchy ::
  WithoutMissingObjects wm es
  => Region wm
  -> Eff es [Region wm]
getRegionHierarchy r = case r ^. #superRegion of
  Nothing -> return []
  Just x -> do
    superR <- getRegion x
    supersR <- getRegionHierarchy superR
    return (superR:supersR)

getEnclosingRegions ::
  WithoutMissingObjects wm es
  => Room wm
  -> Eff es [Region wm]
getEnclosingRegions r = do
  case r ^. #objectData % #containingRegion of
    ContainingRegion Nothing -> pure []
    ContainingRegion (Just reg') -> do
      superR <- getRegion reg'
      supersR <- getRegionHierarchy superR
      return (superR:supersR)
