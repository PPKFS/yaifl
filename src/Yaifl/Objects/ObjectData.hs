module Yaifl.Objects.ObjectData where

import Solitude 
import Yaifl.Common
import Yaifl.Properties.Enclosing

data ThingLit = Lit | NotLit deriving (Eq, Show)

data ThingWearability = NotWearable | Wearable (Maybe Entity) deriving (Eq, Show)

data ThingDescribed = Undescribed | Described deriving (Eq, Show)

-- | Details for things. This is anything tangible.
data ThingData = ThingData
  { _thingContainedBy :: !Entity
  , _thingLit :: !ThingLit
  , _thingWearable :: !ThingWearability
  , _thingDescribed :: !ThingDescribed
  } deriving stock (Generic, Show)

-- | Whether a room has an intrinsic light-ness. This isn't equivalent to whether a
-- room is currently dark - for instance, a cave may have light (if the player has a
-- lantern) but the cave will be Dark.
data Darkness = Lighted | Dark deriving (Eq, Show)

-- | Whether a room has been visited before or not.
data IsVisited = Visited | Unvisited deriving (Eq, Show)

-- | The connections from a one room to another, stored by direction ID.
type MapConnections = Store Entity

-- | An abstract grouping of rooms.
type ContainingRegion = Maybe Entity

-- | Details for room objects. This is anything which is...well, a room. Nontangible.
data RoomData = RoomData
  { _roomIsVisited :: !IsVisited
  , _roomDarkness :: !Darkness
  , _roomMapConnections :: !MapConnections
  , _roomContainingRegion :: !ContainingRegion
  , _roomEnclosing :: !Enclosing
  } deriving stock (Generic, Show)