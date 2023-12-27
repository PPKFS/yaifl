{-|
Module      : Yaifl.Model.Objects.ThingData
Copyright   : (c) Avery 2023
License     : MIT
Maintainer  : ppkfs@outlook.com

Properties that all `Yaifl.Model.Objects.Thing`s have.
-}

module Yaifl.Model.Objects.ThingData (
  -- * Thing data components
    ThingLit(..)
  , ThingWearability(..)
  , ThingDescribed(..)
  , ThingPortable(..)
  , ThingHandled(..)
  , ThingData(..)
  , blankThingData
) where

import Solitude
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Objects.Tag
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

-- | If the thing is able to be picked up or not.
data ThingPortable = Portable | FixedInPlace
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If the thing has been picked up or otherwise interacted with by the player.
data ThingHandled = Handled | NotHandled
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | Properties that define a `Yaifl.Model.Object.Thing`.
data ThingData wm = ThingData
  { containedBy :: EnclosingEntity
  , lit :: ThingLit
  , wearable :: ThingWearability
  , described :: ThingDescribed
  , handled :: ThingHandled
  , portable :: ThingPortable
  , pushableBetweenRooms :: Bool
  , initialAppearance :: WMSayable wm
  } deriving stock (Generic)

deriving stock instance (Eq (WMSayable wm)) => Eq (ThingData wm)
deriving stock instance (Show (WMSayable wm)) => Show (ThingData wm)

makeFieldLabelsNoPrefix ''ThingData

-- | A default thing (when given an initial appearance).
blankThingData :: WMSayable wm -> ThingData wm
blankThingData = ThingData (coerceTag voidID) NotLit NotWearable Described NotHandled Portable True