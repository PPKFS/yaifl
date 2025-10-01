{-|
Module      : Yaifl.Core.Kinds.Object
Copyright   : (c) Avery 2023-2024
License     : MIT
Maintainer  : ppkfs@outlook.com

A game object (a thing or a room).
-}

module Yaifl.Core.Kinds.Object (
  -- * Pointed sets
  Pointed(..)
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

  , makeNameImproper
  , makeNameProper
  ) where

import Yaifl.Prelude
import Yaifl.Core.Entity
import Yaifl.Core.WorldModel (WMText)

-- | If the object has a pluralised name.
data NamePlurality = SingularNamed | PluralNamed
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Read)

-- | If the object should have an indefinite article or not.
data NameProperness = Improper | Proper
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Read)

-- | If the object should have an indefinite article or not.
data NamePrivacy = PrivatelyNamed | PubliclyNamed
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Read)

-- | See also `Yaifl.Core.Metadata.typeDAG`. An object type is just a string that has some relations to other types.
-- there is no data or polymorphism connected to a type, so it's very possible to call something a supporter without
-- having some supporter properties.
newtype ObjectKind = ObjectKind
  { unObjectKind :: Text
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, IsString, Monoid, Semigroup)

-- | A `Timestamp` is used to date events that modify, add, or remove objects.
-- Currently these aren't...used for anything.
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
  , specifics :: objSpecifics -- ^ A @vanilla@ object has no specific additional information; this is a @Pointed@ constraint.
  , objectData :: objData -- ^ `ThingData`, `RoomData`, or `Either ThingData RoomData`.
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''Object

instance Display (WMText wm) => Display (Object wm objData s) where
  displayBuilder o = displayBuilder (o ^. #name)

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

instance HasID (Object wm d s) where
  getID = objectId

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

-- | If something is a thing or a room.
class IsObject o where
  isThing :: o -> Bool

isRoom ::
  IsObject o
  => o
  -> Bool
isRoom = not . isThing

instance {-# OVERLAPPABLE #-} HasID o => IsObject o where
  isThing = isThing . getID

-- | This is safe as long as we only ever generate object IDs under the right principle.
instance IsObject Entity where
  isThing = (> 0) . unID

makeNameImproper :: (WithLabel "nameProperness" NameProperness x, State x :> es) => Eff es ()
makeNameImproper = #nameProperness .= Improper

makeNameProper :: (WithLabel "nameProperness" NameProperness x, State x :> es) => Eff es ()
makeNameProper = #nameProperness .= Proper