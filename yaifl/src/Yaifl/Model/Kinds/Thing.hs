module Yaifl.Model.Kinds.Thing
  ( ThingLit(..)
  , ThingWearability(..)
  , ThingDescribed(..)
  , ThingPortable(..)
  , ThingHandled(..)
  , ThingData(..)
  , blankThingData
  , Thing(..)
  , tagThing
  , defaultPlayerID
  ) where

import Yaifl.Prelude
import Yaifl.Model.Entity
import Yaifl.Model.Tag
import Yaifl.Model.Kinds.Room
import Yaifl.Model.WorldModel
import Yaifl.Model.Kinds.Object
import GHC.Records

-- | If a thing provides light outwards; A lamp is lit, but a closed box with a light inside is not.
data ThingLit = Lit | NotLit
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If a thing is wearable, and if so who (or what) is currently wearing it.
data ThingWearability = NotWearable | Wearable (Maybe Entity)
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | If a thing appears in "You can also see..." paragraphs.
data ThingDescribed = Undescribed | Described
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If the thing is able to be picked up or not.
data ThingPortable = Portable | FixedInPlace
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If the thing has been picked up or otherwise interacted with by the player.
data ThingHandled = Handled | NotHandled
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | Properties that define a `Yaifl.Model.Kinds.Object.Thing`.
data ThingData wm = ThingData
  { containedBy :: EnclosingEntity
  , lit :: ThingLit
  , wearable :: ThingWearability
  , described :: ThingDescribed
  , handled :: ThingHandled
  , portable :: ThingPortable
  , pushableBetweenRooms :: Bool
  , isScenery :: Bool
  , initialAppearance :: WMText wm
  } deriving stock (Generic)

deriving stock instance (Eq (WMText wm)) => Eq (ThingData wm)
deriving stock instance (Show (WMText wm)) => Show (ThingData wm)

makeFieldLabelsNoPrefix ''ThingData

-- | A default thing (when given an initial appearance).
blankThingData :: WMText wm -> ThingData wm
blankThingData = ThingData (coerceTag voidID) NotLit NotWearable Described NotHandled Portable True False

-- | An `Object` with `ThingData`.
newtype Thing wm = Thing (Object wm (ThingData wm) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)

instance HasField x (Object wm (ThingData wm) (WMObjSpecifics wm)) a  => HasField x (Thing wm) a where
  getField (Thing o) = getField @x o

instance Display (Thing wm) where
  displayBuilder = const "thing"

instance HasID (Thing wm) where
  getID (Thing a) = objectId a

-- | The player who is created at the start of the game. This can change (whereas e.g. the Void changing makes no
-- sense) which is why this is named slightly differently.
defaultPlayerID :: TaggedEntity ThingTag
defaultPlayerID = unsafeTagEntity $ Entity 1

instance Taggable (Thing wm) ThingTag

-- | Tag a thing entity.
tagThing ::
  Thing wm
  -> TaggedEntity ThingTag
tagThing r = tag r (r ^. #objectId)

instance IsObject (Thing wm) where
  isThing = const True