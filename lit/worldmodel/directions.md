# Directions

```haskell file=src/Yaifl/Directions.hs
module Yaifl.Directions
  ( WithStandardDirections(..)
  , HasOpposite(..)
  , Direction(..)
  , WithDirections
  , WMStdDirections
  ) where

import Solitude hiding ( Down )
import Yaifl.Common

<<direction-injection>>
<<stock-directions>>
```

```haskell id=direction-injection
class WithStandardDirections d where
  injectDirection :: Direction -> d

class HasOpposite d where
  opposite :: d -> d

type WithDirections (wm :: WorldModel) = (Ord (WMDirections wm), HasOpposite (WMDirections wm))
type WMStdDirections (wm :: WorldModel) = (WithStandardDirections (WMDirections wm), WithDirections wm)
```

```haskell id=stock-directions
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
```