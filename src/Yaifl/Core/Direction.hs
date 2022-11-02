{-|
Module      : Yaifl.Core.Direction
Description : Both an extendable direction framework and the standard compass directions.
Copyright   : (c) Avery 2022
License     : MIT
Maintainer  : ppkfs@outlook.com

This module has 3 parts:

- An extendable way to add new directions in a typesafe manner;
- An implementation of the compass directions;
- Typeclasses for combining the above and also parsing text into directions.
-}

module Yaifl.Core.Direction ( 
  -- * Compass directions
  Direction(..)
  , HasOpposite(..)
  , WithStandardDirections(..)
  , WMStdDirections
  -- * Parsing/Printing
  , HasDirectionalTerms(..)
  ) where

import Yaifl.Core.WorldModel
import Solitude hiding (Down)

-- | A *Direction* is a compass direction, in contrast to a WMDirection (which is probably just compass directions
-- but it may include more).
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

class HasOpposite d where
  opposite :: d -> d

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

-- | A way to inject a `Direction` into a larger Direction type.
class WithStandardDirections d where
  injectDirection :: Direction -> d

instance WithStandardDirections Direction where
  injectDirection = id

-- | A convenient constraint type for everything a direction should be.
type WMStdDirections (wm :: WorldModel) = (
  WithStandardDirections (WMDirection wm)
  , Bounded (WMDirection wm)
  , Enum (WMDirection wm)
  , Show (WMDirection wm)
  , Ord (WMDirection wm)
  , HasOpposite (WMDirection wm))

-- | A way to get all the strings which a direction may be parsed into.
-- This is kind of a human-facing `Show` with multiple options.
-- The `Proxy` argument is needed because of non-injective type family stuff.
class HasDirectionalTerms (wm :: WorldModel) where
  toTextDir :: Proxy wm -> WMDirection wm -> [Text]
  
instance HasDirectionalTerms ('WorldModel s Direction b c) where
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
