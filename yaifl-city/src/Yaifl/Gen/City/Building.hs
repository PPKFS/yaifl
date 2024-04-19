module Yaifl.Gen.City.Building
( Building(..)
, BuildingFloor(..)

) where

import Yaifl.Prelude
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Region
import Yaifl.Game.Create.Object
import Yaifl.Model.WorldModel
import Yaifl.Model.Effects
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Query (isSubregionOf, areInRegion)
import qualified Data.List.NonEmpty as NE
import Yaifl.Model.Kinds.Direction
import Named hiding (Name)
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Game.Create.RoomConnection
import Yaifl.Game.ObjectSpecifics (addDoor, WMHasObjSpecifics)
import Yaifl.Model.MultiLocated
import Language.Haskell.TH
import Data.Char (toUpper)

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
