{-|
Module      : Yaifl.Model.Object
Copyright   : (c) Avery 2023
License     : MIT
Maintainer  : ppkfs@outlook.com

A game object (a thing or a room).
-}

module Yaifl.Model.Object (
  -- * Pointed sets
  Pointed(..)
  -- * Objects
  -- ** Components
  , NamePlurality(..)
  , NameProperness(..)
  -- ** Objects
  , Object(..)
  , Thing(..)
  , Room(..)
  , AnyObject(..)
  , objectEquals
  , isType
  -- ** Tagging
  , tagRoom
  , tagThing
  , TaggedObject(unTagObject)
  , unsafeTagObject
  -- * CanBeAny
  , IsObject(..)
  , CanBeAny(..)
  , _Room
  , _Thing
  ) where

import Solitude

import Data.Set (member)
import Data.Text.Display
import Effectful.Optics ( use )
import GHC.Records

import Yaifl.Metadata
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Objects.RoomData (RoomData)
import Yaifl.Model.Objects.Tag
import Yaifl.Model.Objects.ThingData (ThingData)
import Yaifl.Model.WorldModel (WMObjSpecifics, WMSayable)

-- | Pointed set class; Monoid without the operation, or the dreaded default typeclass.
class Pointed s where
  identityElement :: s

instance {-# OVERLAPPABLE #-} Monoid m => Pointed m where
  identityElement = mempty

-- | Determine whether an object is of a certain type. This is separate to anything on Haskell's side
-- and the type system.
isType ::
  WithMetadata es
  => Is k A_Getter
  => LabelOptic' "objectType" k o ObjectType
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

-- | If the object has a pluralised name.
data NamePlurality = SingularNamed | PluralNamed
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Read)

-- | If the object should have an indefinite article or not.
data NameProperness = Improper | Proper
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Read)

-- | A game object.
data Object wm objData objSpecifics = Object
  { name :: WMSayable wm
  , pluralName :: Maybe (WMSayable wm)
  , indefiniteArticle :: Maybe (WMSayable wm)
  , understandAs :: Set (Set Text)
  , namePlurality :: NamePlurality
  , nameProperness :: NameProperness
  , description :: WMSayable wm
  , objectId :: Entity
  , objectType :: ObjectType
  , creationTime :: Timestamp
  , modifiedTime :: Timestamp
  , specifics :: objSpecifics -- ^ A @vanilla@ object has no specific additional information; this is a @Pointed@ constraint.
  , objectData :: objData -- ^ `ThingData`, `RoomData`, or `Either ThingData RoomData`.
  } deriving stock (Generic)

-- | Because tagging object entities is a headache to marshall between `AnyObject` and not, this is an Object
-- level equivalent to `TaggedEntity`; it allows for witnessed property lookups without `Maybe`.
-- (e.g. a `TaggedObject (Thing wm) DoorTag` can have `Yaifl.Model.Properties.Door.getDoorSpecifics` rather than `Yaifl.Model.Properties.Door.getDoorSpecificsMaybe`).
newtype TaggedObject o tag = TaggedObject { unTagObject :: o }
  deriving stock (Generic)

instance HasID o => HasID (TaggedObject o tag) where
  getID = getID . unTagObject

-- | Unsafely tag an object when we know it's sensible.
unsafeTagObject ::
  o
  -> TaggedObject o tag
unsafeTagObject = TaggedObject

instance HasID o => TaggedAs (TaggedObject o tag) tag where
  toTag = unsafeTagEntity . getID . unTagObject

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

-- | An `Object` with `ThingData`.
newtype Thing wm = Thing (Object wm (ThingData wm) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)
-- | An `Object` with `RoomData`.
newtype Room wm = Room (Object wm (RoomData wm) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)
-- | Either a room or a thing. The `Either` is over the object data so it's easier to
-- do things with the other fields.
newtype AnyObject wm = AnyObject (Object wm (Either (ThingData wm) (RoomData wm)) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)

instance HasField x (Object wm (ThingData wm) (WMObjSpecifics wm)) a  => HasField x (Thing wm) a where
  getField (Thing o) = getField @x o

instance HasField x (Object wm (RoomData wm) (WMObjSpecifics wm)) a  => HasField x (Room wm) a where
  getField (Room o) = getField @x o

instance HasField x (Object wm (Either (ThingData wm) (RoomData wm)) (WMObjSpecifics wm)) a => HasField x (AnyObject wm) a where
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

instance Taggable (Room wm) EnclosingTag
instance Taggable (Room wm) RoomTag
instance Taggable (Thing wm) ThingTag

instance (TaggedAs (Room wm) RoomTag) where
  toTag = tagRoom

instance (TaggedAs (Room wm) EnclosingTag) where
  toTag = coerceTag . tagRoom

-- | Tag a room entity.
tagRoom ::
  Room wm
  -> TaggedEntity RoomTag
tagRoom r = tag r (r ^. #objectId)

-- | Tag a thing entity.
tagThing ::
  Thing wm
  -> TaggedEntity ThingTag
tagThing r = tag r (r ^. #objectId)

instance Functor (Object wm d) where
  fmap f = #specifics %~ f

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

-- | If something is a thing or a room.,
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

-- | This is safe as long as we only ever generate object IDs under the right principle.
instance IsObject Entity where
  isThing = (> 0) . unID