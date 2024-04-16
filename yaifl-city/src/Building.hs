{-# LANGUAGE RecordWildCards #-}
module Building where

import Solitude
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Region
import Yaifl.Game.Create.Object
import Yaifl.Model.WorldModel
import Yaifl.Model.Effects
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Query (isSubregionOf, areInRegion)
import qualified Data.List.NonEmpty as NE
import Yaifl.Model.Kinds.Direction
import Named
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Game.Create.RoomConnection
import Yaifl.Game.ObjectSpecifics (addDoor, WMHasObjSpecifics)
import Yaifl.Model.MultiLocated
import BuildingDefs

data BuildingPlan (m :: Type -> Type) buildingInputs buildingBase buildingComponent building = BuildingPlan
  { makeName :: m Text
  , makeNumberOfComponents :: buildingInputs -> m Int
  , makeBase :: Text -> buildingInputs -> m (buildingBase, buildingComponent)
  , makeComponents :: Text -> buildingInputs -> buildingBase -> NonEmpty buildingComponent -> Int -> m buildingComponent
  , makeBuilding :: Text -> buildingInputs -> buildingBase -> NonEmpty buildingComponent -> m building
  }

class Plannable plan where
  type PlanM plan :: Type -> Type
  type PlanInput plan
  type PlanOutput plan
  runPlan :: Monad (PlanM plan) => plan -> PlanInput plan -> (PlanM plan) (PlanOutput plan)

instance Plannable (BuildingPlan m i ba c b) where
  type PlanM (BuildingPlan m i ba c b) = m
  type PlanInput (BuildingPlan m i ba c b) = i
  type PlanOutput (BuildingPlan m i ba c b) = b
  runPlan BuildingPlan{..} i = do
    n <- makeName
    c <- makeNumberOfComponents i
    (b, bc) <- makeBase n i
    comps <- foldlM (\prev lvl -> flip NE.cons prev <$> makeComponents n i b prev lvl) (bc:|[]) [0..c-1]
    makeBuilding n i b comps

apartmentBuildingPlan ::
  ApartmentFloorGenerator wm es
  => BuildingPlan (Eff es) (Int, Int) RegionEntity (BuildingFloor wm) (Building wm)
apartmentBuildingPlan = BuildingPlan
  { makeName = pure "Apartment Building"
  , makeBase = \name _i -> do
      r <- addRegion name
      entry <- apartmentFloorGen name 0 Nothing
      pure (r, entry)
  , makeNumberOfComponents = \(_a, b) -> pure b
  , makeComponents = \name _i _bRegion (prevFloor:|_others) lvl -> apartmentFloorGen name lvl (Just $ exits prevFloor)
  , makeBuilding = \name _i buildingRegion floors -> return $ Building { name, floors, buildingRegion }
  }

type ApartmentFloorGenerator wm es =
  ( Pointed (WMRegionData wm)
  , ObjectUpdate wm :> es
  , NoMissingObjects wm es
  , WMStdDirections wm
  , WMWithProperty wm MultiLocated
  , WMWithProperty wm Enclosing
  , WMHasObjSpecifics wm
  , AddObjects wm es
  )

apartmentFloorGen ::
  ApartmentFloorGenerator wm es
  => Text
  -> Int
  -> Maybe (RoomEntity, WMDirection wm)
  -> Eff es (BuildingFloor wm)
apartmentFloorGen buildingName lvl mbInputs = do
  -- make the region
  r <- addRegion (buildingName <> ", Floor " <> show lvl)
  r1 <- addRoom (fromString $ "Hallway on level " <> show lvl) ! done
  [r1] `areInRegion` r
  whenJust mbInputs $ \inputs -> do
    r1 `isAbove` (fst inputs)
    addDoor "stairs"
      ! #front inputs
      ! #back (r1, injectDirection $ Yaifl.Model.Kinds.Direction.Down)
      ! done
    pass
  -- make a hallway
  -- make some amount of apartments
  -- connect them to the hallway
  pure $ BuildingFloor
    { level = lvl
    , entrances = 1
    , exits = (r1, injectDirection $ Up)
    , floorRooms = [r1]
    , floorRegion = r
    }