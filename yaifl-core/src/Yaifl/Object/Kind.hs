{-|
Module      : Yaifl.Object.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Core game object system representing both portable things and environmental rooms.

- `Object`: Primary data structure with identification, naming, and kind-specific behavior
- `ObjectKind`: Flexible classification system for object roles/categories
- `Timestamp`: Creation/modification tracking infrastructure
- `Name*` types: Linguistic properties for proper article usage and pluralization
- `IsObject`: Typeclass for thing/room classification
-}

module Yaifl.Object.Kind (
  -- * Objects
  -- ** Components
  NamePlurality(..)
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

-- | Classification type for game objects representing their role or category.
--
-- Forms a DAG (see `Yaifl.Metadata.typeDAG`) allowing flexible classification without
-- inheritance. Enables runtime type checking and hierarchical organization.
--
-- Examples: "container", "supporter", "door", "person", "scenery".
--
-- Simple `Text` wrapper for type safety and kind-specific operations.
newtype ObjectKind = ObjectKind
  { unObjectKind :: Text
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, IsString, Monoid, Semigroup)

-- | A `Timestamp` tracks when objects are created or modified.
-- Timestamps do not necessarily correspond to game turns, but rather to state update ticks.
-- If an object's timestamp is older than the current game state, it indicates the object
-- may be out of date and should be refreshed.
-- Or it would, if it was ever implemented.
newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock (Show, Read, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)

-- | A game object.
data Object wm objData objSpecifics = Object
  { name :: WMText wm -- ^ Primary name of the object
  , pluralName :: Maybe (WMText wm) -- ^ Optional plural form of the name
  , namePrivacy :: NamePrivacy -- ^ Whether the name is publicly accessible to parser
  , indefiniteArticle :: Maybe (WMText wm) -- ^ Optional indefinite article ("a", "an")
  , understandAs :: Set Text -- ^ Alternative text representations for parsing
  , namePlurality :: NamePlurality -- ^ Pluralization behavior (singular/plural)
  , nameProperness :: NameProperness -- ^ Whether name is proper noun
  , description :: WMText wm -- ^ Detailed description of the object
  , objectId :: Entity -- ^ Unique identifier for the object
  , objectType :: ObjectKind -- ^ Classification type of the object
  , creationTime :: Timestamp -- ^ When the object was created
  , modifiedTime :: Timestamp -- ^ When the object was last modified
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

-- | Check if an object is a room (negative entity ID).
--
-- Rooms have negative entity IDs while things have positive IDs.
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
