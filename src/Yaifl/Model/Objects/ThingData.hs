{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Model.Objects.ThingData (
  -- * Thing data
    ThingLit(..)
  , ThingWearability(..)
  , ThingDescribed(..)
  , ThingData(..)
  , blankThingData
  -- ** Optics
  , _Wearable
  , _NotWearable
) where

import Solitude
import Yaifl.Model.Entity ( Entity, voidID )

-- | If a thing provides light outwards; A lamp is lit, but a closed box with a light inside is not.
data ThingLit = Lit | NotLit
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If a thing is wearable, and if so who (or what) is currently wearing it.
data ThingWearability = NotWearable | Wearable (Maybe Entity)
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | If a thing appears in "You can also see..." paragraphs.
data ThingDescribed = Undescribed | Described
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | Properties that define a `Yaifl.Model.Object.Thing`.
data ThingData = ThingData
  { containedBy :: Entity
  , lit :: ThingLit
  , wearable :: ThingWearability
  , described :: ThingDescribed
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | A default thing.
blankThingData :: ThingData
blankThingData = ThingData voidID NotLit NotWearable Described

makePrisms ''ThingWearability