module Yaifl.Spatial.PositionData where

import Yaifl.Prelude
import Rogue.Geometry.V2
import Rogue.Geometry.Rectangle
import Rogue.Tilemap
import Rogue.Array2D.Boxed
import qualified Data.Vector as V

data ThingSpatialData = ThingSpatialData
  { position :: V2
  } deriving stock (Eq, Ord, Generic, Show)

data RoomSpatialData a = RoomSpatialData
  { globalPosition :: V2
  , space :: Array2D a
  } deriving stock (Eq, Ord, Generic, Show)

makeFieldLabelsNoPrefix ''RoomSpatialData

instance Pointed ThingSpatialData where
  identityElement = ThingSpatialData (V2 0 0)

instance Pointed a => Pointed (RoomSpatialData a) where
  identityElement = RoomSpatialData (V2 0 0) (Array2D (V.generate (1*1) (const identityElement), V2 1 1))