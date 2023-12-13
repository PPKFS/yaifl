{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Model.Objects.ThingData (
  -- * Thing data
    ThingLit(..)
  , ThingWearability(..)
  , ThingDescribed(..)
  , ThingData(..)
  , ThingPortable(..)
  , blankThingData
  -- ** Optics
  , _Wearable
  , _NotWearable
) where

import Solitude
import Yaifl.Model.Entity
import Yaifl.Model.WorldModel

-- | If a thing provides light outwards; A lamp is lit, but a closed box with a light inside is not.
data ThingLit = Lit | NotLit
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If a thing is wearable, and if so who (or what) is currently wearing it.
data ThingWearability = NotWearable | Wearable (Maybe Entity)
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | If a thing appears in "You can also see..." paragraphs.
data ThingDescribed = Undescribed | Described
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

data ThingPortable = Portable | FixedInPlace
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | Properties that define a `Yaifl.Model.Object.Thing`.
data ThingData wm = ThingData
  { containedBy :: EnclosingEntity
  , lit :: ThingLit
  , wearable :: ThingWearability
  , described :: ThingDescribed
  , portable :: ThingPortable
  , pushableBetweenRooms :: Bool
  , initialAppearance :: Maybe (WMSayable wm)
  } deriving stock (Generic)

deriving stock instance (Eq (WMSayable wm)) => Eq (ThingData wm)
deriving stock instance (Show (WMSayable wm)) => Show (ThingData wm)

makeFieldLabelsNoPrefix ''ThingData
-- | A default thing.
blankThingData :: ThingData wm
blankThingData = ThingData (coerceTag voidID) NotLit NotWearable Described Portable True Nothing

makePrisms ''ThingWearability