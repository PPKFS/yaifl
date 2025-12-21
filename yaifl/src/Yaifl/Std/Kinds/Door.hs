module Yaifl.Std.Kinds.Door
  ( Door(..)
  , TaggedDoor
  , blankDoor
  , getDoorMaybe
  , tagDoorObject
  , getDoor
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.AnyObject
import Yaifl.Std.Kinds.Openable as O
import Yaifl.Thing.Kind
import Yaifl.Std.Kinds.MultiLocated
import Yaifl.Property.Query
import Yaifl.TH
import Yaifl.Tag

import qualified Data.Set as S
import Yaifl.ObjectLike

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
tagDoorObject = tagObject

getDoor ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Door
  => DoorEntity
  -> Eff es Door
getDoor de = do
  t <- getThing (coerceTag @ThingTag de)
  return $ fromMaybe (error "property witness violated") $ getDoorMaybe t