
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

import Yaifl.Model.Object
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Objects.Tag
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.MultiLocated
import Yaifl.Model.Properties.Openable
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH
import qualified Data.Set as S

data DoorSpecifics = Door
  { isOneWay :: Bool
  , opened :: Openability
  , frontSide :: RoomEntity
  , backSide :: RoomEntity
  , multiLocated :: MultiLocated
  } deriving stock (Eq, Show, Read, Generic)

blankDoorSpecifics :: RoomEntity -> RoomEntity -> DoorSpecifics
blankDoorSpecifics x y = Door False defaultDoorOpenability x y (MultiLocated $ S.fromList [coerceTag x, coerceTag y])

makeFieldLabelsNoPrefix ''DoorSpecifics
makeSpecificsWithout [] ''DoorSpecifics

isClosed ::
  WMWithProperty wm Openability
  => CanBeAny wm o
  => o
  -> Bool
isClosed o = Just Closed == (Yaifl.Model.Properties.Openable.opened <$> getOpenabilityMaybe o)

isOpen ::
  WMWithProperty wm Openability
  => CanBeAny wm o
  => o
  -> Bool
isOpen o = Just Open == (Yaifl.Model.Properties.Openable.opened <$> getOpenabilityMaybe o)

instance Taggable DoorSpecifics DoorTag

type TaggedDoor wm = TaggedObject (Thing wm) DoorTag

tagDoorObject ::
  DoorSpecifics
  -> Thing wm
  -> TaggedObject (Thing wm) DoorTag
tagDoorObject _ds = unsafeTagObject