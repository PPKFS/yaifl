{-# LANGUAGE RecordWildCards #-}
module Yaifl.Gen.City.ApartmentTower
  (

  ) where

import Yaifl.Prelude
import Yaifl.Gen.Plan
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Region
import Yaifl.Game.Create.Object
import Yaifl.Model.WorldModel
import Yaifl.Model.Effects
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Query (areInRegion)
import Yaifl.Model.Kinds.Direction
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Game.Create.RoomConnection
import Yaifl.Game.ObjectSpecifics (addDoor, WMHasObjSpecifics)
import Yaifl.Model.MultiLocated
import Yaifl.Gen.City.Building

data TowerBuildingPlan es inputs base floor (building :: Type) = TowerBuildingPlan
  { baseOptions :: Options es inputs base
  , foyerOptions :: Options2 es inputs base floor
  , floorOptions :: SequentialOptions3 es inputs base floor floor
  , build :: inputs -> base -> NonEmpty floor -> Eff es building
  }

instance LabelOptic' "numberOfFloors" A_Lens ba Int  => Plannable (TowerBuildingPlan es i ba f b) where
  type PlanM (TowerBuildingPlan es i ba f b) = Eff es
  type PlanInput (TowerBuildingPlan es i ba f b) = i
  type PlanOutput (TowerBuildingPlan es i ba f b) = b
  runPlan TowerBuildingPlan{..} i = do
    b <- pickOne baseOptions i
    f <- pickOne foyerOptions (i, b)
    fs <- pickSequential (b ^. #numberOfFloors) (i, b, f) floorOptions
    build i b (f:|fs)

type ApartmentTowerBase = (Text, RegionEntity, Int)
type ApartmentFoyerPlan es i b f = PlanOption es (f, (i, b)) f
type ApartmentFloorPlan es i b f = PlanOption es (f, (Int, (i, b, f), [f])) f
type ApartmentTowerPlan es wm = TowerBuildingPlan es Int ApartmentTowerBase (BuildingFloor wm) (Building wm)

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

apartmentTowerPlan :: ApartmentFloorGenerator wm es => ApartmentTowerPlan es wm
apartmentTowerPlan =  TowerBuildingPlan
  { baseOptions = equalWeights $ one makeBuildingBase
  , foyerOptions = beforePlanWith makeFloor $ equalWeights $ fromList [ smallFoyer1Staircase, longFoyer2Staircases ]
  , floorOptions = beforePlanWith makeFloor $ equalWeights $ fromList [ singleApartmentFloor, landing2Apartment, landing4Apartment, hallway6Apartment ]
  , build = \_i (name, buildingRegion, _) floors -> return $ Building { name, floors, buildingRegion }
  }

singleApartmentFloor :: ApartmentFloorPlan es Int ApartmentTowerBase (BuildingFloor wm)
singleApartmentFloor = error ""

landing2Apartment :: ApartmentFloorPlan es Int ApartmentTowerBase (BuildingFloor wm)
landing2Apartment = error ""

landing4Apartment :: ApartmentFloorPlan es Int ApartmentTowerBase (BuildingFloor wm)
landing4Apartment = error ""

hallway6Apartment :: ApartmentFloorPlan es Int ApartmentTowerBase (BuildingFloor wm)
hallway6Apartment = error ""

smallFoyer1Staircase :: ApartmentFoyerPlan es Int ApartmentTowerBase (BuildingFloor wm)
smallFoyer1Staircase = error ""

longFoyer2Staircases :: ApartmentFoyerPlan es Int ApartmentTowerBase (BuildingFloor wm)
longFoyer2Staircases = error ""

makeFloor :: (x -> Eff es c)
makeFloor = error ""

makeBuildingBase :: PlanOption es Int ApartmentTowerBase
makeBuildingBase = error ""


{-}
  { makeName = pure "Apartment Building"
  , makeBase = \name _i -> do
      r <- addRegion name
      entry <- apartmentFloorGen name 0 Nothing
      pure (r, entry)
  , makeComponents = \name _i _bRegion (prevFloor:|_others) lvl -> apartmentFloorGen name lvl (Just $ exits prevFloor)
  , makeBuilding = \name _i buildingRegion floors -> return $ Building { name, floors, buildingRegion }
  }
-}


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