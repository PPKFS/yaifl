{-|
Module      : Yaifl.Core.Object
Description : A game object.
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

Not everything is an object (like in Inform), but anything that can be interacted with or goes through the motions
is an object. Namely, there's no need for e.g. directions to be objects.
-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Object (
  -- * Objects
  Object(..)
  , Thing
  , Room
  , AnyObject
  -- ** Helpers
  , objectEquals
  , isType
  , IsObject(..)
  , CanBeAny(..)
  -- ** Optics
  , _Room
  , _Thing
  ) where

import Solitude

import Data.Set (member)
import Effectful.Optics ( use )

import Yaifl.Core.Entity (Entity (..), HasID (..))
import Yaifl.Core.Metadata ( ObjectType, noteError, typeDAG, Timestamp, WithMetadata )
import Yaifl.Core.Objects.RoomData (RoomData)
import Yaifl.Core.Objects.ThingData (ThingData)
import Yaifl.Core.WorldModel (WMObjSpecifics, WMSayable)
import Data.Text.Display

-- | Determine whether an object is of a certain type.
isType ::
  WithMetadata es
  => Object wm d -- ^ The object.
  -> ObjectType -- ^ The type.
  -> Eff es Bool
isType o = isTypeInternal (objectType o)
  where
    isTypeInternal ::
      WithMetadata es
      => ObjectType
      -> ObjectType
      -> Eff es Bool
    isTypeInternal obj e' = do
      td <- use $ #typeDAG % at obj
      case td of
        Nothing -> noteError (const False) ("Found no type entry for " <> show obj)
        Just iv ->
          if
            e' `member` iv || obj == e'
          then
            return True
          else
            anyM (`isTypeInternal` e') iv

-- | A game object.
data Object wm objData = Object
  { name :: WMSayable wm
  , description :: WMSayable wm
  , objectId :: Entity
  , objectType :: ObjectType
  , creationTime :: Timestamp
  , specifics :: Maybe (WMObjSpecifics wm) -- ^ A 'vanilla' object has no specific additional information.
  , objectData :: objData -- ^ `ThingData`, `RoomData`, or `Either ThingData RoomData`.
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''Object

instance Display (Object wm objData) where
  displayBuilder = const "object"

instance HasID (Object wm d) where
  getID = objectId

type Thing wm = Object wm ThingData
type Room wm = Object wm (RoomData wm)
type AnyObject wm = Object wm (Either ThingData (RoomData wm))

-- | By generalising `Eq`, we can compare two objects of different kinds. Trivially this is always `False`,
-- but it does allow comparing a `Thing` and an `AnyObject`.
objectEquals ::
  Object wm d
  -> Object wm d'
  -> Bool
objectEquals = (. objectId) . (==) . objectId

instance Eq (Object wm d) where
  (==) = objectEquals

-- | Maybe I'll need this instance for something or other?
instance Ord (Object wm d) where
  compare = (. creationTime) . compare . creationTime

instance Functor (Object wm) where
  fmap ::
    (a -> b)
    -> Object wm a
    -> Object wm b
  fmap f = #objectData %~ f

instance Foldable (Object wm) where
  foldMap ::
    (a -> m)
    -> Object wm a
    -> m
  foldMap f = f . objectData

instance Traversable (Object wm) where
  traverse ::
    Applicative f
    => (a -> f b)
    -> Object wm a
    -> f (Object wm b)
  traverse f o = flip (set #objectData) o <$> f (objectData o)

-- | A prism for getting a `Room` out of an `AnyObject`.
_Room :: Prism' (AnyObject wm) (Room wm)
_Room = prism' (fmap Right) (traverse rightToMaybe)

-- | A prism for getting a `Thing` out of an `AnyObject`.
_Thing :: Prism' (AnyObject wm) (Thing wm)
_Thing = prism' (fmap Left) (traverse leftToMaybe)

-- | A slightly more descriptive prism for objects specifically.
class CanBeAny wm o where
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

class IsObject o where
  isThing :: o -> Bool
  isRoom :: o -> Bool
  default isRoom :: o -> Bool
  isRoom = not . isThing

instance IsObject (Room wm) where
  isThing = const False
  isRoom = const True

instance IsObject (Thing wm) where
  isThing = const True
  isRoom = const False

instance IsObject (AnyObject wm) where
  isThing = isJust . fromAny @wm @(Thing wm)

instance IsObject Entity where
  isThing = (> 0) . unID