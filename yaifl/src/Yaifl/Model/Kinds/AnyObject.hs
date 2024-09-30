module Yaifl.Model.Kinds.AnyObject
  ( IsObject(..)
  , CanBeAny(..)
  , AnyObject(..)
  , _Room
  , _Thing
  , TaggedEnclosing
  ) where

import Yaifl.Prelude

import GHC.Records
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Kinds.Room
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.WorldModel
import Yaifl.Model.Tag
import Yaifl.Model.Kinds.Enclosing

type RawAnyObject wm = Object wm (Either (ThingData wm) (RoomData wm)) (WMObjSpecifics wm)
-- | Either a room or a thing. The `Either` is over the object data so it's easier to
-- do things with the other fields.
newtype AnyObject wm = AnyObject (RawAnyObject wm)
  deriving newtype (Eq, Ord, Generic)

instance HasField x (RawAnyObject wm) a => HasField x (AnyObject wm) a where
  getField (AnyObject o) = getField @x o

instance Display (WMText wm) => Display (AnyObject wm) where
  displayBuilder o = displayBuilder (o ^. #name)

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

type TaggedEnclosing wm = TaggedObject (AnyObject wm) EnclosingTag

instance TaggedAs (AnyObject wm, Enclosing) EnclosingTag where
  toTag (o, e) = tag e o

