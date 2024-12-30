module Yaifl.Model.Kinds.Door
  ( Door(..)
  , TaggedDoor
  , blankDoor
  , getDoorMaybe
  , tagDoorObject
  ) where

import Yaifl.Prelude

import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Core.HasProperty
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Model.Kinds.Openable as O
import Yaifl.Core.Kinds.Thing
import Yaifl.Model.MultiLocated
import Yaifl.Model.Query
import Yaifl.Model.TH
import Yaifl.Core.Tag

import qualified Data.Set as S

data Door = Door
  { isOneWay :: Bool
  , opened :: Openability
  , frontSide :: RoomEntity
  , backSide :: RoomEntity
  , multiLocated :: MultiLocated
  } deriving stock (Eq, Show, Read, Generic)

blankDoor ::
  RoomEntity
  -> RoomEntity
  -> Door
blankDoor x y = Door False defaultDoorOpenability x y (MultiLocated $ S.fromList [coerceTag x, coerceTag y])

makeFieldLabelsNoPrefix ''Door
makeSpecificsWithout [] ''Door

instance Taggable Door DoorTag

type TaggedDoor wm = TaggedObject (Thing wm) DoorTag

tagDoorObject ::
  Door
  -> Thing wm
  -> TaggedObject (Thing wm) DoorTag
tagDoorObject _ds = unsafeTagObject