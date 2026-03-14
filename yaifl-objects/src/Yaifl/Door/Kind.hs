{-|
Module      : Yaifl.Door.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Doors represent connections between rooms that can be opened, closed,
and potentially locked to control movement between spaces.

This module defines the `Door` type and its associated components:

- `Door`: The core door type with connection and state information
- `TaggedDoor`: Type-safe reference to door objects
- Functions for creating and manipulating doors
-}

module Yaifl.Door.Kind
  ( -- * Door types
    Door(..)
  , TaggedDoor

    -- * Door functions
  , blankDoor
  , getDoorMaybe
  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.AnyObject
import Yaifl.Openable.Kind as O hiding (opened)
import Yaifl.Thing.Kind
import Yaifl.MultiLocated.Kind
import Yaifl.Property.Query
import Yaifl.Tag

import qualified Data.Set as S
import Yaifl.TH (makeGetMaybe, WMWithProperty)

-- | A connection between rooms that can control movement.
-- Doors have two sides (front and back) and can be one-way or bidirectional.
-- They integrate with the openable system to support opening, closing, and locking.
data Door = Door
  { isOneWay :: Bool
    -- ^ Whether the door only allows movement in one direction
  , opened :: Openability
    -- ^ Current open/closed/locked state
  , frontSide :: RoomEntity
    -- ^ Primary side of the door
  , backSide :: RoomEntity
    -- ^ Secondary side of the door
  , multiLocated :: MultiLocated
    -- ^ Location data for multi-room presence
  } deriving stock (Eq, Show, Read, Generic)

-- | Create a basic door between two rooms.
-- The door is initially bidirectional, closed but unlockable (default openability).
blankDoor ::
  RoomEntity
  -> RoomEntity
  -> Door
blankDoor x y = Door False defaultDoorOpenability x y (MultiLocated $ S.fromList [coerceTag x, coerceTag y])

makeFieldLabelsNoPrefix ''Door
makeGetMaybe ''Door

instance Taggable Door DoorTag

type TaggedDoor wm = TaggedObject (Thing wm) DoorTag

instance CouldBeOpened Door where
  hasOpenability = Just . opened