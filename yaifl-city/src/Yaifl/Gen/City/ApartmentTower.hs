{-# LANGUAGE RecordWildCards #-}
module Yaifl.Gen.City.ApartmentTower
  ( apartmentTowerPlan

  ) where

import Yaifl.Prelude hiding (State, Down)
import Yaifl.Gen.Plan
import Yaifl.Model.Kinds.Region
import Yaifl.Game.Create.Object
import Yaifl.Model.WorldModel
import Yaifl.Model.Query (isSubregionOf)
import Yaifl.Model.Kinds.Direction
import Yaifl.Game.Create.RoomConnection
import Yaifl.Game.ObjectSpecifics (addDoor)
import Yaifl.Gen.City.Apartment
import Yaifl.Gen.City.Building
import Control.Placeholder (pattern TODO, todo)
import GHC.TypeLits
import Yaifl.Text.DynamicText
import Yaifl.Model.Kinds (Room, getRoom)
import Yaifl.Model.Entity
import Yaifl.Model.Rules (RuleEffects)
import Effectful.State.Static.Local
import Yaifl.Model.Effects (setRoom)

data TowerBuildingPlan es inputs base floor (building :: Type) = TowerBuildingPlan
  { baseOptions :: Options es inputs base
  , foyerOptions :: Options es base floor
  , floorOptions :: SequentialOptions2 es base floor floor
  , build :: base -> NonEmpty floor -> Eff es building
  }

instance LabelOptic' "numberOfFloors" A_Lens ba Int => Plannable (TowerBuildingPlan es i ba f b) where
  type PlanM (TowerBuildingPlan es i ba f b) = Eff es
  type PlanInput (TowerBuildingPlan es i ba f b) = i
  type PlanOutput (TowerBuildingPlan es i ba f b) = b
  runPlan TowerBuildingPlan{..} i = do
    b <- pickOne baseOptions i
    f <- pickOne foyerOptions b
    fs <- pickSequential (b ^. #numberOfFloors) (b, f) floorOptions
    build b (f:|fs)

type ApartmentFoyerPlan es b f = PlanOption es (RegionEntity, b) f
type ApartmentFloorPlan es b f = PlanOption es (RegionEntity, (Int, (b, f), [f])) f
type ApartmentTowerPlan es wm = TowerBuildingPlan es (WMDirection wm, Int) (ApartmentTowerBase wm) (BuildingFloor wm) (Building wm)

data FloorBase wm = FloorBase
  { floorNumber :: Int
  , towerBase :: ApartmentTowerBase wm
  } deriving stock (Generic)

apartmentTowerPlan :: BuildingGeneration wm es => ApartmentTowerPlan es wm
apartmentTowerPlan = TowerBuildingPlan
  { baseOptions = equalWeights $ one makeBuildingBase
  , foyerOptions = beforePlanWith (makeFloor . FloorBase 0) $ equalWeights $ fromList
      [ smallFoyer1Staircase
      --, longFoyer2Staircases
      ]
  , floorOptions = beforePlanWith (\(i, (b, _), _) -> makeFloor (FloorBase i b)) $ equalWeights $ fromList
      [  landing2Apartment
      --, singleApartmentFloor
      --, landing4Apartment  n jhmmmmmmh
      --, hallway6Apartment
      ]
  , build = \apb@ApartmentTowerBase{..} floors -> return $ Building { name, floors, buildingBase = apb }
  }

{-
data ApartmentPlan1 = Foyer1 Staircase Floor
-}
singleApartmentFloor :: ApartmentFloorPlan es (ApartmentTowerBase wm) (BuildingFloor wm)
singleApartmentFloor = todo

landing2Apartment :: BuildingGeneration wm es => ApartmentFloorPlan es (ApartmentTowerBase wm) (BuildingFloor wm)
landing2Apartment (floorRegion, (floorNum, (building, foyer), prevFloors)) = do
  r1 <- addRoom (fromString $ toString $ (building ^. #name) <> ", Floor " <> show floorNum <> "; Hallway")
    ! #description "The hallway landing is threadbare, with a clearly worn trail across the carpet towards the two apartment doors."
    ! done
  let belowFloor = fromMaybe foyer (viaNonEmpty head prevFloors) ^. #exits % _1
  addDoor "staircase"
    ! #front (belowFloor, injectDirection $ Up)
    ! #back (r1, injectDirection $ Down)
    ! done
  ap1 <- runPlan apartmentPlan (todo, 1, 1)
  pure $ BuildingFloor
    { level = 0
    , entrances = 1
    , exits = (r1, injectDirection $ Up)
    , floorRooms = [r1]
    , floorRegion = floorRegion
    }

landing4Apartment :: ApartmentFloorPlan es (ApartmentTowerBase wm) (BuildingFloor wm)
landing4Apartment = todo

hallway6Apartment :: ApartmentFloorPlan es (ApartmentTowerBase wm) (BuildingFloor wm)
hallway6Apartment = todo

data RoomSize = Small | Medium | Large

foyerDescription ::
  "size" :? RoomSize
  -> "wayOut" :? WMDirection wm
  -> "stairsUp" :? WMDirection wm
  -> DynamicText wm
foyerDescription (argF #size -> s) (argF #wayOut -> wo) (argF #stairsUp -> su) = todo

smallFoyer1Staircase :: forall wm es. BuildingGeneration wm es => ApartmentFoyerPlan es (ApartmentTowerBase wm) (BuildingFloor wm)
smallFoyer1Staircase (floorRegion, building) = do
  -- if the entrance of the building is on the WEST of the building, then going EAST from outside should go into the building
  -- and going WEST out of the building
  let wayOut = building ^. #entranceIsOnFace
  -- TODO: this should come in from the building inputs
  outside <- addRoom "Outside"
    ! #description ("It's a little chilly in the winter air. To the " <> (show $ opposite $ building ^. #entranceIsOnFace) <> " is a big apartment tower.")
    ! done
  r1 <- addRoom (fromString $ toString $ (building ^. #name) <> ", Foyer")
    ! #description "The foyer is small."--(foyerDescription @wm ! #size Small ! #wayOut wayOut ! #stairsUp (opposite wayOut) ! done)
    ! done

  -- TODO: this is annoying
  addDirectionFrom (building ^. #entranceIsOnFace) outside r1
  addDoor "big wooden door"
    ! #front (outside, opposite wayOut)
    ! #back (r1, wayOut)
    ! done
  pure $ BuildingFloor
    { level = 0
    , entrances = 1
    , exits = (r1, injectDirection $ Up)
    , floorRooms = [r1]
    , floorRegion = floorRegion
    }

longFoyer2Staircases :: ApartmentFoyerPlan es (ApartmentTowerBase wm) (BuildingFloor wm)
longFoyer2Staircases = TODO

type With (n :: Symbol) i o = LabelOptic' n A_Lens i o

makeFloor ::
  With "floorNumber" i Int
  => With "towerBase" i (ApartmentTowerBase wm)
  => BuildingGeneration wm es
  => i -> Eff es RegionEntity
makeFloor f = do
  r <- addRegion (f ^. #towerBase % #name <> ", Floor " <> show (f ^. #floorNumber))
  r `isSubregionOf` (f ^. #towerBase % #region)
  pure $ r

makeBuildingBase :: BuildingGeneration wm es => PlanOption es (WMDirection wm, Int) (ApartmentTowerBase wm)
makeBuildingBase (entranceIsOnFace, numberOfFloors) = do
  let name = "Apartment Building"
  region <- addRegion "Apartment Building"
  return ApartmentTowerBase {entranceIsOnFace, region, numberOfFloors, name}

data BlueprintKind = Shell | Construction
type Plan = WorldModel -> BlueprintKind -> Type
newtype BlueprintAction wm component plan = BA { runIt :: forall es. BuildingGeneration wm es => plan -> Eff (State component : es) () }

type family RoomBlueprint (wm :: WorldModel) (bk :: BlueprintKind) (p :: Plan)
type instance RoomBlueprint wm 'Shell p = RoomEntity
type instance RoomBlueprint wm 'Construction p  = BlueprintAction wm (Room wm) (p wm 'Shell)

data SpokePlan wm a = SpokePlan
  { centre :: RoomBlueprint wm a SpokePlan
  , top :: RoomBlueprint wm a SpokePlan
  , left :: RoomBlueprint wm a SpokePlan
  , right :: RoomBlueprint wm a SpokePlan
  }

class Blueprintable (a :: Plan) where
  constructBlueprint :: forall wm es. BuildingGeneration wm es => ({- todo: orientation/northIs -}) -> a wm 'Construction -> Eff es (a wm 'Shell)

class BlueprintComponent wm componentEntity component where
  buildAction :: BuildingGeneration wm es => plan -> BlueprintAction wm component plan -> componentEntity -> Eff es ()

instance BlueprintComponent wm RoomEntity (Room wm) where
  buildAction :: BuildingGeneration wm es => plan -> BlueprintAction wm (Room wm) plan -> RoomEntity -> Eff es ()
  buildAction sp b r = do
    room <- getRoom r
    r' <- execState room (runIt b sp)
    setRoom r'

instance Blueprintable SpokePlan where
  constructBlueprint :: BuildingGeneration wm es => ({- todo: orientation/northIs -}) -> SpokePlan wm Construction -> Eff es (SpokePlan wm Shell)
  constructBlueprint _ SpokePlan{..} = do
    r1 <- addRoom "centre" ! done
    r2 <- addRoom "top" ! done
    r3 <- addRoom "left" ! done
    r4 <- addRoom "right" ! done
    r1 `isSouthOf` r2
    r3 `isWestOf` r1
    r4 `isEastOf` r1
    let sp = SpokePlan @_ @'Shell r1 r2 r3 r4
    mapM_ (uncurry (buildAction sp)) $ zip [centre, top, left, right] [r1, r2, r3, r4]
    pure sp
