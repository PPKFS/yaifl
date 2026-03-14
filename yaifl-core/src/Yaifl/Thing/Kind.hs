{-|
Module      : Yaifl.Thing.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Things represent objects in the game world that can be manipulated or interacted with,
including portable items, fixed objects, and environmental features.

This module defines the `Thing` type and its associated data structures:

- `Thing`: The core thing type wrapping `Object` with thing-specific data (see `Yaifl.Object.Kind`)
- `ThingData`: Comprehensive properties for thing behaviour and state
- `Thing*` types: Individual property types (providing light, wearability, portability, etc.)
- Functions for querying thing properties and state

See also:
- `Yaifl.Object.Kind` for the base Object type
- `Yaifl.Enclosing.Kind` for enclosing functionality
- `Yaifl.Tag` for the tagging system used by `tagThingEntity`
-}

module Yaifl.Thing.Kind
  ( -- * Thing property types
    ThingLit(..)
  , ThingWearability(..)
  , ThingDescribed(..)
  , ThingPortable(..)
  , ThingHandled(..)

    -- * Thing data
  , ThingData(..)
  , blankThingData

    -- * Thing type
  , Thing(..)

    -- * Enclosing tagging
    -- | Things are contained by enclosing objects and require enclosing entity references
  , tagThingEntity
  , EnclosingThing

    -- * Default values
  , defaultPlayerID

    -- * Property queries
  , thingIsLit
  , thingIsWorn
  , thingIsConcealed
  , thingIsScenery
  , thingIsDescribed
  , thingIsHandled
  , thingIsPortable
  , thingIsPushable
  , thingContainedBy
  ) where

import Yaifl.Prelude

import GHC.Records
import Yaifl.Entity
import Yaifl.Object.Kind
import Yaifl.Room.Kind
import Yaifl.Tag
import Yaifl.WorldModel

-- | If a thing provides light outwards; A lamp is lit, but a closed box with a light inside is not.
data ThingLit = Lit | NotLit
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If a thing is wearable, and if so who (or what) is currently wearing it.
data ThingWearability = NotWearable | Wearable (Maybe ThingEntity)
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | If a thing appears in "You can also see..." paragraphs.
data ThingDescribed = Undescribed | Described
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If the thing is able to be picked up or not.
data ThingPortable = Portable | FixedInPlace
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If the thing has been picked up or otherwise interacted with by the player. Things which have been handled
-- will no longer display their initial appearance in descriptions.
data ThingHandled = Handled | NotHandled
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If a thing is concealed, and if so who (or what) is currently concealing it.
data ThingConcealed = NotConcealed | Concealed (Maybe ThingEntity)
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | Properties that define a `Yaifl.Object.Kind.Thing`.
data ThingData wm = ThingData
  { containedBy :: EnclosingEntity
  , lit :: ThingLit
  , wearable :: ThingWearability
  , described :: ThingDescribed
  , handled :: ThingHandled
  , portable :: ThingPortable
  , pushableBetweenRooms :: Bool
  , isScenery :: Bool
  , concealed :: ThingConcealed
  , thingData :: WMThingData wm
  , initialAppearance :: WMText wm

  } deriving stock (Generic)

deriving stock instance (Eq (WMText wm), Eq (WMThingData wm)) => Eq (ThingData wm)
deriving stock instance (Show (WMText wm), Show (WMThingData wm)) => Show (ThingData wm)

makeFieldLabelsNoPrefix ''ThingData
makePrismLabels ''ThingWearability
makePrismLabels ''ThingConcealed

-- | A default thing (when given an initial appearance).
blankThingData :: Pointed (WMThingData wm) => WMText wm -> ThingData wm
blankThingData = ThingData (coerceTag voidID) NotLit NotWearable Described NotHandled Portable True False NotConcealed identityElement

-- | A thing object with thing-specific data and behaviour.
--
-- Wraps an `Object` (from `Yaifl.Object.Kind`) with `ThingData`, providing access to thing properties
-- such as lighting, wearability, portability, and containment through the
-- `HasField` instance. Maintains compatibility with the object system via
-- `HasEntity` and `IsObject` instances.
newtype Thing wm = Thing (Object wm (ThingData wm) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)

instance HasField x (Object wm (ThingData wm) (WMObjSpecifics wm)) a  => HasField x (Thing wm) a where
  getField (Thing o) = getField @x o

instance Display (WMText wm) => Display (Thing wm) where
  displayBuilder = displayBuilder . coerce @_ @(Object wm (ThingData wm) (WMObjSpecifics wm))

instance HasEntity (Thing wm) where
  getEntity (Thing a) = objectId a

-- | The player who is created at the start of the game. This can change (whereas e.g. the Void changing makes no
-- sense) which is why this is named slightly differently.
defaultPlayerID :: TaggedEntity PersonTag
defaultPlayerID = unsafeTagEntity $ Entity 1

type EnclosingThing wm = TaggedObject (Thing wm) EnclosingTag

instance Taggable (Thing wm) ThingTag

-- | Tag a thing with its entity for type-safe references.
-- Uses the thing's object ID to create a tagged entity reference.
tagThingEntity ::
  Thing wm
  -> TaggedEntity ThingTag
tagThingEntity r = tagEntity r (r ^. #objectId)

instance IsObject (Thing wm) where
  isThing = const True

-- | Check if a thing is currently providing light.
thingIsLit ::
  Thing wm
  -> Bool
thingIsLit = (== Lit) . view (#objectData % #lit)

-- | Check if a thing is currently being worn.
thingIsWorn ::
  Thing wm
  -> Bool
thingIsWorn = isJust . preview (#objectData % #wearable % #_Wearable)

-- | Check if a thing is concealed (hidden from descriptions and examination).
thingIsConcealed ::
  Thing wm
  -> Bool
thingIsConcealed = isJust . preview (#objectData % #concealed % #_Concealed)

-- | Check if a thing is marked as scenery.
thingIsScenery ::
  Thing wm
  -> Bool
thingIsScenery = view (#objectData % #isScenery)

-- | Get the enclosing object that contains a thing.
thingContainedBy ::
  Thing wm
  -> EnclosingEntity
thingContainedBy = view (#objectData % #containedBy)

-- | Check if a thing appears in "You can also see..." paragraphs.
thingIsDescribed ::
  Thing wm
  -> Bool
thingIsDescribed = (== Described) . view (#objectData % #described)

-- | Check if a thing has been handled (interacted with by player).
thingIsHandled ::
  Thing wm
  -> Bool
thingIsHandled = (== Handled) . view (#objectData % #handled)

-- | Check if a thing is portable (can be picked up).
thingIsPortable ::
  Thing wm
  -> Bool
thingIsPortable = (== Portable) . view (#objectData % #portable)

-- | Check if a thing can be pushed between rooms.
thingIsPushable ::
  Thing wm
  -> Bool
thingIsPushable = view (#objectData % #pushableBetweenRooms)