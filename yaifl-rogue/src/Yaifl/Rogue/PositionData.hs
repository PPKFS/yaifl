module Yaifl.Rogue.PositionData where

import Yaifl.Prelude
import Rogue.Geometry.V2
import Rogue.Array2D.Boxed
import qualified Data.Vector as V
import Rogue.Colour

data Renderable = Renderable
  { glyph :: Char
  , foreground :: Colour
  , background :: Colour
  } deriving stock (Show, Read, Generic, Eq, Ord)

data ThingSpatialData = ThingSpatialData
  { position :: V2
  , renderable :: Renderable
  } deriving stock (Eq, Ord, Generic, Show)

data RoomSpatialData a = RoomSpatialData
  { globalPosition :: V2
  , space :: Array2D a
  } deriving stock (Eq, Ord, Generic, Show)

makeFieldLabelsNoPrefix ''RoomSpatialData

instance Pointed ThingSpatialData where
  identityElement = ThingSpatialData (V2 0 0) (Renderable '@' (Colour 0xFF000000) (Colour 0x00000000))

instance Pointed a => Pointed (RoomSpatialData a) where
  identityElement = RoomSpatialData (V2 0 0) (Array2D (V.generate (1*1) (const identityElement), V2 1 1))