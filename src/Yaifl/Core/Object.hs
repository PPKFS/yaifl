{-|
Module      : Yaifl.Core.Object
Description : A game object.
Copyright   : (c) Avery 2022
License     : MIT
Maintainer  : ppkfs@outlook.com

Not everything is an object (like in Inform), but anything that can be interacted with or goes through the motions
is an object. Namely, there's no need for e.g. directions to be objects.
-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData #-}

module Yaifl.Core.Object (
  -- * Objects
  Object(..)
  , Thing
  , Room
  , AnyObject
  , ObjectDomain
  , AdaptiveObjectText
  -- ** Helpers
  , objectEquals
  , isType
  , IsObject(..)
  , CanBeAny(..)
  -- ** Optics
  , objName
  , objDescription
  , objID
  , objType
  , objCreationTime
  , objSpecifics
  , objData
  , _Room
  , _Thing
  ) where

import Solitude

import Data.Set (member)
import Effectful.Optics ( use )

import Yaifl.Core.AdaptiveText ( AdaptiveText, AdaptiveTextDomain )
import Yaifl.Core.Entity (Entity (..), HasID (..))
import Yaifl.Core.Logger (Log)
import Yaifl.Core.Metadata ( ObjType, Metadata, noteError, typeDAG, Timestamp )
import Yaifl.Core.Objects.RoomData (RoomData)
import Yaifl.Core.Objects.ThingData (ThingData)
import Yaifl.Core.WorldModel (WMObjSpecifics, WorldModel (..))

-- | Determine whether an object is of a certain type.
isType ::
  State Metadata :> es
  => Log :> es
  => Object wm d -- ^ The object.
  -> ObjType -- ^ The type.
  -> Eff es Bool
isType o = isTypeInternal (_objType o)
  where
    isTypeInternal obj e' = do
      td <- use $ typeDAG % at obj
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
  { _objName :: AdaptiveText (ObjectDomain wm)
  , _objDescription :: AdaptiveText (ObjectDomain wm)
  , _objID :: Entity
  , _objType :: ObjType
  , _objCreationTime :: Timestamp
  , _objSpecifics :: Maybe (WMObjSpecifics wm) -- ^ A 'vanilla' object has no specific additional information.
  , _objData :: objData -- ^ `ThingData`, `RoomData`, or `Either ThingData RoomData`.
  } deriving stock (Generic)

instance Display (Object wm objData) where
  displayBuilder = const "object"

instance HasID (Object wm d) where
  getID = _objID

type Thing wm = Object wm ThingData
type Room wm = Object wm (RoomData wm)
type AnyObject wm = Object wm (Either ThingData (RoomData wm))

data ObjectDomain (wm :: WorldModel)

type AdaptiveObjectText wm = AdaptiveText (ObjectDomain wm)
type instance AdaptiveTextDomain (ObjectDomain wm) = AnyObject wm

-- | By generalising `Eq`, we can compare two objects of different kinds. Trivially this is always `False`,
-- but it does allow comparing a `Thing` and an `AnyObject`.
objectEquals ::
  Object wm d
  -> Object wm d'
  -> Bool
objectEquals = (. _objID) . (==) . _objID

instance Eq (Object wm d) where
  (==) = objectEquals

-- | Maybe I'll need this instance for something or other?
instance Ord (Object wm d) where
  compare = (. _objCreationTime) . compare . _objCreationTime

makeLenses ''Object

instance Functor (Object wm) where
  fmap ::
    (a -> b)
    -> Object wm a
    -> Object wm b
  fmap f = objData %~ f

instance Foldable (Object wm) where
  foldMap ::
    (a -> m)
    -> Object wm a
    -> m
  foldMap f = f . _objData

instance Traversable (Object wm) where
  traverse ::
    Applicative f
    => (a -> f b)
    -> Object wm a
    -> f (Object wm b)
  traverse f o = flip (set objData) o <$> f (_objData o)

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