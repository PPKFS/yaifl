-- ~\~ language=Haskell filename=src/Yaifl/Core/Directions.hs
-- ~\~ begin <<lit/worldmodel/directions.md|src/Yaifl/Core/Directions.hs>>[0] project://lit/worldmodel/directions.md:4
module Yaifl.Core.Directions
  ( WithStandardDirections(..)
  , HasOpposite(..)
  , Direction(..)
  , WMParseableDirections(..)
  , WithDirections
  , WMStdDirections
  ) where

import Yaifl.Core.Common
import Prelude hiding (Down)

-- ~\~ begin <<lit/worldmodel/directions.md|direction-injection>>[0] project://lit/worldmodel/directions.md:20
class WithStandardDirections d where
  injectDirection :: Direction -> d

class HasOpposite d where
  opposite :: d -> d

type WithDirections (wm :: WorldModel) = (Ord (WMDirections wm), HasOpposite (WMDirections wm))
type WMStdDirections (wm :: WorldModel) = (WithStandardDirections (WMDirections wm), WithDirections wm, Bounded (WMDirections wm), Enum (WMDirections wm), Show (WMDirections wm))
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/directions.md|stock-directions>>[0] project://lit/worldmodel/directions.md:31
data Direction =
  North
  | South
  | East
  | West
  | NorthWest
  | NorthEast
  | SouthWest
  | SouthEast
  | In
  | Out
  | Up
  | Down
  deriving stock (Eq, Show, Read, Ord, Enum, Generic, Bounded)

instance WithStandardDirections Direction where
  injectDirection = id

instance HasOpposite Direction where
  opposite = \case
    North -> South
    South -> North
    West -> East
    East -> West
    NorthWest -> SouthEast
    NorthEast -> SouthWest
    SouthEast -> NorthWest
    SouthWest -> NorthEast
    In -> Out
    Out -> In
    Up -> Down
    Down -> Up

parseStandardDirections :: Text -> Maybe Direction
parseStandardDirections = readMaybe . toString

class WMParseableDirections (wm :: WorldModel) where
  toTextDir :: Proxy wm -> WMDirections wm -> [Text]

instance WMParseableDirections ('WorldModel s Direction b c) where
  toTextDir _ = \case
    North -> ["n", "north"]
    South -> ["s", "south"]
    West -> ["w", "west"]
    East -> ["e", "east"]
    NorthWest -> ["nw", "northwest", "north-west", "north west"]
    NorthEast -> ["ne", "northeast", "north-east", "north east"]
    SouthEast -> ["se", "southeast", "south-east", "south east"]
    SouthWest -> ["sw", "southwest", "south-west", "south west"]
    In -> ["nw", "northwest", "north-west", "north west"]
    Out -> ["nw", "northwest", "north-west", "north west"]
    Up -> ["nw", "northwest", "north-west", "north west"]
    Down -> ["nw", "northwest", "north-west", "north west"]
-- ~\~ end
-- ~\~ end
