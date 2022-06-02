-- ~\~ language=Haskell filename=src/Yaifl/Directions.hs
-- ~\~ begin <<lit/worldmodel/directions.md|src/Yaifl/Directions.hs>>[0] project://lit/worldmodel/directions.md:5
module Yaifl.Directions where

import Solitude hiding (Down)
import Yaifl.Common

class WithStandardDirections d where
  injectDirection :: Direction -> d


type WithDirections (wm :: WorldModel) = (Ord (WMDirections wm), HasOpposite (WMDirections wm))
type WMStdDirections (wm :: WorldModel) = (WithStandardDirections (WMDirections wm), WithDirections wm)

class HasOpposite d where
  opposite :: d -> d

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
-- ~\~ end
