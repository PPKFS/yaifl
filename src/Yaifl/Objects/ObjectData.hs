{-|
Module      : Yaifl.Objects.ObjectData
Description : Properties specific to either `Thing`s or `Room`s.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Objects.ObjectData
  ( -- * Things
    ThingLit(..)
  , ThingWearability(..)
  , ThingDescribed(..)
  , ThingData(..)
  , blankThingData

  -- * Rooms
  , MapConnections(..)
  , ContainingRegion(..)
  , Darkness(..)
  , RoomData(..)
  , blankRoomData
  
  -- * Lenses
  , thingContainedBy
  , thingLit
  , thingWearable
  , thingDescribed
  , roomIsVisited
  , roomDarkness
  , roomMapConnections
  , roomContainingRegion
  , roomEnclosing
  , _Wearable
  ) where

import Solitude 
import Yaifl.Common
import Yaifl.Properties.Enclosing

-- | If a thing provides light outwards; A lamp is lit, but a closed box with a light inside is not.
data ThingLit = Lit | NotLit 
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If a thing is wearable, and if so who (or what) is currently wearing it.
data ThingWearability = NotWearable | Wearable (Maybe Entity) 
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | If a thing appears in "You can also see..." paragraphs.
data ThingDescribed = Undescribed | Described 
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | Details for things. This is anything tangible.
data ThingData = ThingData
  { _thingContainedBy :: Entity
  , _thingLit :: ThingLit
  , _thingWearable :: ThingWearability
  , _thingDescribed :: ThingDescribed
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | Default thing data.
blankThingData :: ThingData
blankThingData = ThingData defaultVoidID NotLit NotWearable Described

-- | Whether a room has an intrinsic light-ness. This isn't equivalent to whether a
-- room is currently dark - for instance, a cave may have light (if the player has a
-- lantern) but the cave will be Dark.
data Darkness = Lighted | Dark 
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | Whether a room has been visited before or not.
data IsVisited = Visited | Unvisited 
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | The connections from a one room to another, stored by direction ID.
newtype MapConnections = MapConnections
  { unMapConnections :: Store Entity 
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, Generic)

-- | An abstract grouping of rooms.
newtype ContainingRegion = ContainingRegion
  { unRegion :: Maybe Entity
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, Generic)

-- | Details for room objects. This is anything which is...well, a room. Nontangible.
data RoomData = RoomData
  { _roomIsVisited :: IsVisited
  , _roomDarkness :: Darkness
  , _roomMapConnections :: MapConnections
  , _roomContainingRegion :: ContainingRegion
  , _roomEnclosing :: Enclosing
  } deriving stock (Eq, Show, Read, Ord, Generic)

blankRoomData :: RoomData
blankRoomData = RoomData Unvisited Lighted (MapConnections emptyStore) (ContainingRegion Nothing) blankEnclosing

makeLenses ''ThingData
makeLenses ''RoomData

makePrisms ''ThingWearability