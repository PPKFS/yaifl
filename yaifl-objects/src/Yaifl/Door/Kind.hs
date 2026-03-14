module Yaifl.Door.Kind
  ( Door(..)
  , TaggedDoor
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
makeGetMaybe ''Door

instance Taggable Door DoorTag

type TaggedDoor wm = TaggedObject (Thing wm) DoorTag

instance CouldBeOpened Door where
  hasOpenability = Just . opened