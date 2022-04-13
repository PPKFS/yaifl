module Yaifl.Directions where

import Solitude hiding (Down)
import Yaifl.Common

class WithStandardDirections d where
  injectDirection :: Direction -> d

type WMHasDirections (wm :: WorldModel) = WithStandardDirections (WMDirections wm)
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
