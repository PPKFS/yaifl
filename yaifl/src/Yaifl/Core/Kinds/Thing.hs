module Yaifl.Core.Kinds.Thing
  ( ThingLit(..)
  , ThingWearability(..)
  , ThingDescribed(..)
  , ThingPortable(..)
  , ThingHandled(..)
  , ThingData(..)
  , blankThingData

  , Thing(..)
  , tagThingEntity
  , EnclosingThing
  , defaultPlayerID
  , thingIsLit
  , thingIsWorn
  , thingIsConcealed
  , thingIsScenery
  , makeItScenery
  , thingContainedBy
  ) where

import Yaifl.Prelude

import GHC.Records
import Yaifl.Core.Entity
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Tag
import Yaifl.Core.WorldModel

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

-- | Properties that define a `Yaifl.Core.Kinds.Object.Thing`.
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

-- | An `Object` with `ThingData`.
newtype Thing wm = Thing (Object wm (ThingData wm) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)

instance HasField x (Object wm (ThingData wm) (WMObjSpecifics wm)) a  => HasField x (Thing wm) a where
  getField (Thing o) = getField @x o

instance Display (WMText wm) => Display (Thing wm) where
  displayBuilder = displayBuilder . coerce @_ @(Object wm (ThingData wm) (WMObjSpecifics wm))

instance HasID (Thing wm) where
  getID (Thing a) = objectId a

-- | The player who is created at the start of the game. This can change (whereas e.g. the Void changing makes no
-- sense) which is why this is named slightly differently.
defaultPlayerID :: TaggedEntity PersonTag
defaultPlayerID = unsafeTagEntity $ Entity 1

type EnclosingThing wm = TaggedObject (Thing wm) EnclosingTag

instance Taggable (Thing wm) ThingTag

-- | Tag a thing entity.
tagThingEntity ::
  Thing wm
  -> TaggedEntity ThingTag
tagThingEntity r = tagEntity r (r ^. #objectId)

instance IsObject (Thing wm) where
  isThing = const True

thingIsLit ::
  Thing wm
  -> Bool
thingIsLit = (== Lit) . view (#objectData % #lit)

thingIsWorn ::
  Thing wm
  -> Bool
thingIsWorn = isJust . preview (#objectData % #wearable % #_Wearable)

thingIsConcealed ::
  Thing wm
  -> Bool
thingIsConcealed = isJust . preview (#objectData % #concealed % #_Concealed)

thingIsScenery ::
  Thing wm
  -> Bool
thingIsScenery = view (#objectData % #isScenery)

makeItScenery :: Eff '[State (Thing wm)] ()
makeItScenery = (#objectData % #isScenery .= True)

thingContainedBy ::
  Thing wm
  -> EnclosingEntity
thingContainedBy = view (#objectData % #containedBy)