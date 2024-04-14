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

data Building wm = Building
  { name :: Text
  , floors :: NonEmpty (BuildingFloor wm)
  , buildingRegion :: RegionEntity
  }

data BuildingFloor wm = BuildingFloor
  { level :: Int
  , entrances :: Int
  , exits :: (RoomEntity, WMDirection wm)
  , floorRooms :: [RoomEntity]
  , floorRegion :: RegionEntity
  }
deriving stock instance Show (WMDirection wm) => Show (Building wm)
deriving stock instance Show (WMDirection wm) => Show (BuildingFloor wm)

data BuildingPlan (m :: Type -> Type) buildingInputs buildingBase buildingComponent building = BP
  { makeName :: m Text
  , makeNumberOfComponents :: buildingInputs -> m Int
  , makeBase :: Text -> buildingInputs -> m (buildingBase, buildingComponent)
  , makeComponents :: Text -> buildingInputs -> buildingBase -> NonEmpty buildingComponent -> Int -> m buildingComponent
  , makeBuilding :: Text -> buildingInputs -> buildingBase -> NonEmpty buildingComponent -> m building
  }

runPlan ::
  Monad m
  => BuildingPlan m i ba c b
  -> i
  -> m b
runPlan BP{..} i = do
  n <- makeName
  c <- makeNumberOfComponents i
  (b, bc) <- makeBase n i
  comps <- foldlM (\prev lvl -> flip NE.cons prev <$> makeComponents n i b prev lvl) (bc:|[]) [0..c-1]
  makeBuilding n i b comps

apartmentBuildingPlan ::
  Pointed (WMRegionData wm)
  => ObjectUpdate wm :> es
  => NoMissingObjects wm es
  => WMStdDirections wm
  => WMWithProperty wm MultiLocated
  => WMWithProperty wm Enclosing
  => WMHasObjSpecifics wm
  => AddObjects wm es
  => BuildingPlan (Eff es) (Int, Int) RegionEntity (BuildingFloor wm) (Building wm)
apartmentBuildingPlan = BP
  { makeName = pure "Apartment Building"
  , makeBase = \name _i -> do
      r <- addRegion name
      entry <- apartmentFloorGen name 0 Nothing
      pure (r, entry)
  , makeNumberOfComponents = \(_a, b) -> pure b
  , makeComponents = \name _i _bRegion (prevFloor:|_others) lvl -> apartmentFloorGen name lvl (Just $ exits prevFloor)
  , makeBuilding = \name _i buildingRegion floors -> return $ Building { name, floors, buildingRegion }
  }

apartmentFloorGen :: Pointed (WMRegionData wm)
  => NoMissingObjects wm es
  => WMStdDirections wm
  => WMWithProperty wm MultiLocated
  => WMWithProperty wm Enclosing
  => WMHasObjSpecifics wm
  => AddObjects wm es
  => Text -> Int -> Maybe (RoomEntity, WMDirection wm) -> Eff es (BuildingFloor wm)
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

{-
a recursive generator is:
- you have some generic construction.
-- you have some amount of the sub-thing to generate, or an end.
-- the sub thing gets a long chain of everything that was passed in


a sub thing can either be: sequential, or independent

-}
{-
a building:
- has a name and other various properties
- these are actually a property of the building REGION
- then some amount of floors

a floor:
inherits its name *probably* from the building
- again, a region property
- each floor should have some connection up (possibly) and some connection down (possibly)
or rather, some amounts of entries (inputs) and some amount of exits outputs
where the i/o model is defined by (->). well, it's annoying to have it be in haskell land
but the floor generator should produce something with N exits and feed that N to the
required number of inputs for the next floor (where "exits" are staircases)
and entries are doors for the ground floor and staircases elsewhere

a floor should be constructed of a number of entry rooms.

I think we need to have a generic way to build a *building* and then pass in whether it's an apartment or whatever
-}