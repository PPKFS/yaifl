module Yaifl.Model.Kinds.AnyObject
  ( IsObject(..)
  , CanBeAny(..)
  , AnyObject(..)
  , _Room
  , _Thing

  ) where

import Solitude
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Kinds.Room
import Yaifl.Model.Entity
import Yaifl.Model.WorldModel
import GHC.Records
import Data.Text.Display

-- | Either a room or a thing. The `Either` is over the object data so it's easier to
-- do things with the other fields.
newtype AnyObject wm = AnyObject (Object wm (Either (ThingData wm) (RoomData wm)) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)

instance HasField x (Object wm (Either (ThingData wm) (RoomData wm)) (WMObjSpecifics wm)) a => HasField x (AnyObject wm) a where
  getField (AnyObject o) = getField @x o

instance Display (AnyObject wm) where
  displayBuilder = const "object"

instance HasID (AnyObject wm) where
  getID (AnyObject a) = objectId a

-- | A prism for getting a `Room` out of an `AnyObject`.
_Room :: Prism' (AnyObject wm) (Room wm)
_Room = prism' (AnyObject . first Right . coerce) (fmap Room . bitraverse rightToMaybe Just . (\(AnyObject a) -> a))

-- | A prism for getting a `Thing` out of an `AnyObject`.
_Thing :: Prism' (AnyObject wm) (Thing wm)
_Thing = prism' (AnyObject . first Left . coerce) (fmap Thing . bitraverse leftToMaybe Just . (\(AnyObject a) -> a))

-- | A slightly more descriptive prism for objects specifically.
class CanBeAny wm o | o -> wm where
  toAny :: o -> AnyObject wm
  fromAny :: AnyObject wm -> Maybe o

instance CanBeAny wm (Room wm) where
  toAny = review _Room
  fromAny = preview _Room

instance CanBeAny wm (Thing wm) where
  toAny = review _Thing
  fromAny = preview _Thing

instance CanBeAny wm (AnyObject wm) where
  toAny = id
  fromAny = Just

instance IsObject (AnyObject wm) where
  isThing = isJust . fromAny @wm @(Thing wm)
