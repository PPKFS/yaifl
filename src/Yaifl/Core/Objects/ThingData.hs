{-|
Module      : Yaifl.Core.Objects.ThingData
Description : Properties that define a `Yaifl.Core.Object.Thing`.
Copyright   : (c) Avery 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Objects.ThingData (
  -- * Thing data
    ThingLit(..)
  , ThingWearability(..)
  , ThingDescribed(..)
  , ThingData(..)
  , blankThingData
  -- ** Optics
  , thingContainedBy
  , thingLit
  , thingWearable
  , thingDescribed
  , _Wearable
  , _NotWearable
) where

import Solitude
import Yaifl.Core.Entity ( Entity, voidID )

-- | If a thing provides light outwards; A lamp is lit, but a closed box with a light inside is not.
data ThingLit = Lit | NotLit
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If a thing is wearable, and if so who (or what) is currently wearing it.
data ThingWearability = NotWearable | Wearable (Maybe Entity)
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | If a thing appears in "You can also see..." paragraphs.
data ThingDescribed = Undescribed | Described
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | Properties that define a `Yaifl.Core.Object.Thing`.
data ThingData = ThingData
  { _thingContainedBy :: Entity
  , _thingLit :: ThingLit
  , _thingWearable :: ThingWearability
  , _thingDescribed :: ThingDescribed
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | A default thing.
blankThingData :: ThingData
blankThingData = ThingData voidID NotLit NotWearable Described

makeLenses ''ThingData
makePrisms ''ThingWearability