{-|
Module      : Yaifl.Model.Object
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
{-# LANGUAGE DeriveAnyClass #-}

module Yaifl.Model.Object (
  -- * Objects
  Object(..)
  , Pointed(..)
  , Thing(..)
  , Room(..)
  , AnyObject(..)
  , NamePlurality(..)
  , NameProperness(..)
  -- ** Helpers
  , objectEquals
  , isType
  , IsObject(..)
  , CanBeAny(..)
  -- ** Optics
  , _Room
  , _Thing

  , RoomTag
  , ThingTag
  , tagRoom
  , tagThing
  ) where

import Solitude

import Data.Set (member)
import Effectful.Optics ( use )

import Yaifl.Model.Entity
import Yaifl.Metadata
import Yaifl.Model.Objects.RoomData (RoomData)
import Yaifl.Model.Objects.ThingData (ThingData)
import Yaifl.Model.WorldModel (WMObjSpecifics, WMSayable)
import Data.Text.Display
import GHC.Records

class Pointed s where
  identityElement :: s

-- | Determine whether an object is of a certain type.
isType ::
  WithMetadata es
  => LabelOptic' "objectType" A_Lens o ObjectType
  => o -- ^ The object.
  -> ObjectType -- ^ The type.
  -> Eff es Bool
isType o = isTypeInternal (o ^. #objectType)
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

data NamePlurality = SingularNamed | PluralNamed
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Read)

data NameProperness = Improper | Proper
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Read)

-- | A game object.
data Object wm objData objSpecifics = Object
  { name :: WMSayable wm
  , pluralName :: Maybe (WMSayable wm)
  , indefiniteArticle :: Maybe (WMSayable wm)
  , namePlurality :: NamePlurality
  , nameProperness :: NameProperness
  , description :: WMSayable wm
  , objectId :: Entity
  , objectType :: ObjectType
  , creationTime :: Timestamp
  , modifiedTime :: Timestamp
  , specifics :: objSpecifics -- ^ A 'vanilla' object has no specific additional information.
  , objectData :: objData -- ^ `ThingData`, `RoomData`, or `Either ThingData RoomData`.
  } deriving stock (Generic)


-- | By generalising `Eq`, we can compare two objects of different kinds. Trivially this is always `False`,
-- but it does allow comparing a `Thing` and an `AnyObject`.
objectEquals ::
  HasID a
  => HasID b
  => a
  -> b
  -> Bool
objectEquals = (. getID) . (==) . getID

instance Eq (Object wm d s) where
  (==) = objectEquals

-- | Maybe I'll need this instance for something or other?
instance Ord (Object wm d s) where
  compare = (. creationTime) . compare . creationTime

newtype Thing wm = Thing (Object wm ThingData (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)
newtype Room wm = Room (Object wm (RoomData wm) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)
newtype AnyObject wm = AnyObject (Object wm (Either ThingData (RoomData wm)) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)



instance HasField x (Object wm ThingData (WMObjSpecifics wm)) a  => HasField x (Thing wm) a where
  getField (Thing o) = getField @x o

instance HasField x (Object wm (RoomData wm) (WMObjSpecifics wm)) a  => HasField x (Room wm) a where
  getField (Room o) = getField @x o

instance HasField x (Object wm (Either ThingData (RoomData wm)) (WMObjSpecifics wm)) a => HasField x (AnyObject wm) a where
  getField (AnyObject o) = getField @x o

instance Display (Object wm objData s) where
  displayBuilder = const "object"

instance Display (Room wm) where
  displayBuilder = const "room"

instance Display (Thing wm) where
  displayBuilder = const "thing"

instance Display (AnyObject wm) where
  displayBuilder = const "object"

instance HasID (Object wm d s) where
  getID = objectId

instance HasID (AnyObject wm) where
  getID (AnyObject a) = objectId a

instance HasID (Thing wm) where
  getID (Thing a) = objectId a

instance HasID (Room wm) where
  getID (Room a) = objectId a

makeFieldLabelsNoPrefix ''Object

instance Taggable (Room wm) RoomTag
instance Taggable (Thing wm) ThingTag

tagRoom ::
  Room wm
  -> TaggedEntity RoomTag
tagRoom r = tag r (r ^. #objectId)

tagThing ::
  Thing wm
  -> TaggedEntity ThingTag
tagThing r = tag r (r ^. #objectId)

instance Bifunctor (Object wm) where
  bimap f g o = o & #objectData %~ f & #specifics %~ g

instance Bifoldable (Object wm) where
  bifoldMap f g o = f (o ^. #objectData) <> g (o ^. #specifics)

instance Bitraversable (Object wm) where
  bitraverse f g o =
    let d' = f (objectData o)
        s' = g (specifics o)
    in (\d s -> o & #objectData .~ d & #specifics .~ s) <$> d' <*> s'

-- | A prism for getting a `Room` out of an `AnyObject`.
_Room :: Prism' (AnyObject wm) (Room wm)
_Room = prism' (AnyObject . first Right . coerce) (fmap Room . bitraverse rightToMaybe Just . (\(AnyObject a) -> a))

-- | A prism for getting a `Thing` out of an `AnyObject`.
_Thing :: Prism' (AnyObject wm) (Thing wm)
_Thing = prism' (AnyObject . first Left . coerce) (fmap Thing . bitraverse leftToMaybe Just . (\(AnyObject a) -> a))

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