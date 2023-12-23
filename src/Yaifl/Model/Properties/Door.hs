
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Model.Properties.Door
  ( DoorSpecifics(..)
  , TaggedDoor
  , blankDoorSpecifics
  , getDoorSpecificsMaybe
  , isOpen
  , isClosed
  , tagDoorObject
  ) where

import Solitude

import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Properties.Openable
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Object
import Yaifl.Model.Properties.MultiLocated
import qualified Data.Set as S
import Yaifl.Model.Objects.Tag

data DoorSpecifics = Door
  { isOneWay :: Bool
  , openable :: Openable
  , frontSide :: RoomEntity
  , backSide :: RoomEntity
  , multiLocated :: MultiLocated
  } deriving stock (Eq, Show, Read, Generic)

blankDoorSpecifics :: RoomEntity -> RoomEntity -> DoorSpecifics
blankDoorSpecifics x y = Door False Closed x y (MultiLocated $ S.fromList [coerceTag x, coerceTag y])

makeFieldLabelsNoPrefix ''DoorSpecifics
makeSpecificsWithout [] ''DoorSpecifics

isClosed ::
  WMWithProperty wm Openable
  => CanBeAny wm o
  => o
  -> Bool
isClosed o = Just Closed == getOpenableMaybe o

isOpen ::
  WMWithProperty wm Openable
  => CanBeAny wm o
  => o
  -> Bool
isOpen o = Just Open == getOpenableMaybe o

instance Taggable DoorSpecifics DoorTag

type TaggedDoor wm = TaggedObject (Thing wm) DoorTag

tagDoorObject ::
  DoorSpecifics
  -> Thing wm
  -> TaggedObject (Thing wm) DoorTag
tagDoorObject _ds = unsafeTagObject