{-|
Module      : Yaifl.Object.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Game objects represent the fundamental building blocks of the game world,
encompassing both things (portable objects) and rooms (environmental locations).

This module defines the core `Object` data structure and its supporting components:

- `Object`: The primary data structure combining identification, naming, and kind-specific behaviour
- `ObjectKind`: Classification system for object kinds (distinct from Haskell types)
- `Timestamp`: Creation and modification tracking for objects
- `Name*` types: Linguistic properties controlling article usage and pluralisation
- `IsObject`: Typeclass for thing/room classification
-}

module Yaifl.Object.Kind (
  -- * Objects
  -- ** Components
  , NamePlurality(..)
  , NameProperness(..)
  , NamePrivacy(..)
  -- ** Objects
  , Object(..)
  , Timestamp(..)
  , ObjectKind(..)
  , IsObject(..)
  , objectEquals
  , isRoom
  ) where

import Yaifl.Prelude
import Yaifl.Entity
import Yaifl.WorldModel (WMText)

-- | Whether the object's name changes when pluralised.
-- `SingularNamed` objects like "key" become "keys" when plural.
-- `PluralNamed` objects like "sheep" remain "sheep" when plural.
data NamePlurality = SingularNamed | PluralNamed
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Read)

-- | Whether the object's name is a proper noun or common noun.
-- `Proper` names (e.g., "John", "London") don't use articles.
-- `Improper` names (e.g., "key", "door") may use indefinite articles.
data NameProperness = Improper | Proper
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Read)

-- | Whether the object's name is accessible to the command parser.
-- `PrivatelyNamed` objects cannot be referred to by their given name in commands.
-- They can only be referenced via their `understandAs` properties.
-- `PubliclyNamed` objects can be referred to directly by their name in commands.
data NamePrivacy = PrivatelyNamed | PubliclyNamed
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Read)

-- | A classification type for game objects, representing their role or category in the game world.
-- ObjectKinds form a directed acyclic graph (see `Yaifl.Metadata.typeDAG`) where relationships
-- between types are explicitly defined rather than implied through inheritance or polymorphism.
--
-- This design allows flexible classification: an object can be classified as a "supporter" without
-- requiring any specific data structure or behavioural implementation. The type system is purely
-- descriptive, enabling runtime type checking and hierarchical organisation without code coupling.
--
-- Examples of object kinds include: "container", "supporter", "door", "person", "scenery".
--
-- The `ObjectKind` is a simple wrapper around `Text` for type safety and to enable specific
-- instances and operations related to the type system.
newtype ObjectKind = ObjectKind
  { unObjectKind :: Text
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, IsString, Monoid, Semigroup)

-- | A `Timestamp` tracks when objects are created or modified.
-- Timestamps do not necessarily correspond to game turns, but rather to state update ticks.
-- If an object's timestamp is older than the current game state, it indicates the object
-- may be out of date and should be refreshed.
--
-- Currently implemented but not actively used in game logic, this field provides
-- infrastructure for potential future features such as object aging, undo functionality,
-- or temporal queries.
newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock (Show, Read, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)

-- | A game object.
data Object wm objData objSpecifics = Object
  { name :: WMText wm
  , pluralName :: Maybe (WMText wm)
  , namePrivacy :: NamePrivacy
  , indefiniteArticle :: Maybe (WMText wm)
  , understandAs :: Set Text
  , namePlurality :: NamePlurality
  , nameProperness :: NameProperness
  , description :: WMText wm
  , objectId :: Entity
  , objectType :: ObjectKind
  , creationTime :: Timestamp
  , modifiedTime :: Timestamp
  , specifics :: objSpecifics -- ^ Kind-specific data (e.g., door mechanics, container contents)
  , objectData :: objData -- ^ `ThingData`, `RoomData`, or `Either ThingData RoomData`.
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''Object

instance Display (WMText wm) => Display (Object wm objData s) where
  displayBuilder o = displayBuilder (o ^. #name)

-- | By generalising `Eq`, we can compare two objects of different kinds. Trivially this is always `False`,
-- but it does allow comparing a `Thing` and an `AnyObject`.
objectEquals ::
  HasEntity a
  => HasEntity b
  => a
  -> b
  -> Bool
objectEquals = (. getEntity) . (==) . getEntity

instance Eq (Object wm d s) where
  (==) = objectEquals

-- | Order objects by creation time for chronological sorting.
instance Ord (Object wm d s) where
  compare = (. creationTime) . compare . creationTime

instance HasEntity (Object wm d s) where
  getEntity = objectId

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

-- | Typeclass for determining whether an object is a thing or a room.
-- This provides a uniform interface for thing/room classification across
-- different object representations.
--
-- The classification follows the convention established in `Yaifl.Entity`:
-- positive entity IDs represent `Yaifl.Thing.Kind.Thing`s, while negative
-- entity IDs represent `Yaifl.Room.Kind.Room`s.
class IsObject o where
  isThing :: o -> Bool

isRoom ::
  IsObject o
  => o
  -> Bool
isRoom = not . isThing

instance {-# OVERLAPPABLE #-} HasEntity o => IsObject o where
  isThing = isThing . getEntity

-- | This is safe as long as we only ever generate object IDs under the right principle.
instance IsObject Entity where
  isThing = (> 0) . unEntity
