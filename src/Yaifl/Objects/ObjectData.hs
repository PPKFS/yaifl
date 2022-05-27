-- ~\~ language=Haskell filename=src/Yaifl/Objects/ObjectData.hs
-- ~\~ begin <<lit/worldmodel/objects/data.md|src/Yaifl/Objects/ObjectData.hs>>[0] project://lit/worldmodel/objects/data.md:4
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
  , Explicitness(..)
  , Connection(..)
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
  --, roomEnclosing
  , _Wearable

  , connectionExplicitness
  , connectionRoom
  ) where

import Solitude 
import Yaifl.Common
--import Yaifl.Properties.Enclosing
import qualified Data.Map as Map

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

data Explicitness = Explicit | Implicit 
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

data Connection = Connection 
  { _connectionExplicitness :: Explicitness
  , _connectionRoom :: Entity
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | The connections from a one room to another, stored by direction ID.
newtype MapConnections wm = MapConnections
  { unMapConnections :: Map.Map (WMDirections wm) Connection 
  }

deriving newtype instance (Generic (Map (WMDirections wm) Connection)) => Generic (MapConnections wm)
deriving newtype instance (Ord (WMDirections wm)) => Ord (MapConnections wm)
deriving newtype instance (Read (WMDirections wm), Ord (WMDirections wm)) => Read (MapConnections wm)
deriving newtype instance (Show (WMDirections wm)) => Show (MapConnections wm)
deriving stock instance (Eq (WMDirections wm)) => Eq (MapConnections wm)

-- | An abstract grouping of rooms.
newtype ContainingRegion = ContainingRegion
  { unRegion :: Maybe Entity
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, Generic)

-- | Details for room objects. This is anything which is...well, a room. Nontangible.
data RoomData wm = RoomData
  { _roomIsVisited :: IsVisited
  , _roomDarkness :: Darkness
  , _roomMapConnections :: MapConnections wm
  , _roomContainingRegion :: ContainingRegion
  --, _roomEnclosing :: Enclosing
  } deriving stock (Generic)

deriving stock instance (Ord (WMDirections wm)) => Ord (RoomData wm)
deriving stock instance (Read (WMDirections wm), Ord (WMDirections wm)) => Read (RoomData wm)
deriving stock instance (Show (WMDirections wm)) => Show (RoomData wm)
deriving stock instance (Eq (WMDirections wm)) => Eq (RoomData wm)

blankRoomData :: RoomData wm
blankRoomData = RoomData Unvisited Lighted (MapConnections Map.empty) (ContainingRegion Nothing) -- blankEnclosing

makeLenses ''ThingData
makeLenses ''RoomData
makeLenses ''Connection

makePrisms ''ThingWearability
-- ~\~ end
