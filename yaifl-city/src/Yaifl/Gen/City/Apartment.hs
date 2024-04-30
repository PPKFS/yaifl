{-# LANGUAGE RecordWildCards #-}
module Yaifl.Gen.City.Apartment
  ( apartmentPlan

  ) where

import Yaifl.Prelude
import Yaifl.Gen.Plan
import Control.Placeholder
import Yaifl.Model.WorldModel
import Yaifl.Model.Kinds.Region
import Yaifl.Game.Create
import Yaifl.Model.Entity
import Yaifl.Model.Rules
import Yaifl.Gen.City.Building

data RoomType = LivingRoom | Kitchen | Bathroom | Bedroom | Study
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

data ApartmentPlan es inputs base room (apartment :: Type) = ApartmentPlan
  { baseOptions :: Options es inputs base
  , hallwayOptions :: Options es base room
  , roomOptions :: RoomType -> Options es base room
  , build :: base -> NonEmpty room -> Eff es apartment
  }

type ApartmentFoyerPlan es b f = PlanOption es (RegionEntity, b) f
type ApartmentFloorPlan es b f = PlanOption es (RegionEntity, (Int, (b, f), [f])) f
type ApartmentPlan' es wm = ApartmentPlan es (WMDirection wm, Int, Int) (ApartmentBase wm) (ApartmentRoom wm) (Apartment wm)

data ApartmentBase (wm :: WorldModel) = ApartmentBase
  { name :: Text
  , number :: Int
  }
data ApartmentRoom wm
data Apartment wm

apartmentPlan :: BuildingGeneration wm es => ApartmentPlan' es wm
apartmentPlan = ApartmentPlan
  { baseOptions = oneOption makeApartmentBase
  , hallwayOptions = oneOption makeLongHallway
  , roomOptions = \rt -> beforePlanWith (\v -> makeApartmentRoom rt v) $ equalWeights $ case rt of
      LivingRoom -> one makeLivingRoom
      Kitchen -> one makeKitchen
      Bathroom -> one makeBathroom
      Bedroom -> one makeLivingRoom
      Study -> one makeStudy
  , build = \base rooms -> do
      error ""
  }

makeStudy :: (RoomEntity, ApartmentBase wms) -> Eff es (ApartmentRoom wms)
makeStudy = todo

makeBathroom :: (RoomEntity, ApartmentBase wms) -> Eff es (ApartmentRoom wms)
makeBathroom = todo

makeKitchen :: (RoomEntity, ApartmentBase wms) -> Eff es (ApartmentRoom wms)
makeKitchen = todo

makeLivingRoom :: (RoomEntity, ApartmentBase wms) -> Eff es (ApartmentRoom wms)
makeLivingRoom = todo

makeApartmentRoom :: BuildingGeneration wm es => RoomType -> ApartmentBase wm -> Eff es RoomEntity
makeApartmentRoom rt base = do
  let name = makeApartmentRoomName rt base
  addRoom name ! done

makeApartmentRoomName :: IsString (WMSayable wm) => RoomType -> ApartmentBase wm -> WMSayable wm
makeApartmentRoomName rt (ApartmentBase{name}) = fromString $ toString $ name <> " " <> show rt

makeLongHallway :: PlanOption es (ApartmentBase wms) (ApartmentRoom wms)
makeLongHallway = todo

makeApartmentBase :: PlanOption es (WMDirection wms, Int, Int) (ApartmentBase wms)
makeApartmentBase = todo

instance Plannable (ApartmentPlan es i b r a) where
  type PlanM (ApartmentPlan es i b r a) = Eff es
  type PlanInput (ApartmentPlan es i b r a) = i
  type PlanOutput (ApartmentPlan es i b r a) = a
  runPlan ApartmentPlan{..} i = do
    b <- pickOne baseOptions i
    h <- pickOne hallwayOptions b
    rs <- pickOne (roomOptions LivingRoom) b
    build b (h:|[rs])