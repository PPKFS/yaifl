# ObjectData

This is where we differentiate between `Thing`s and `Room`s. Object data is those properties which are common (and then objects are further specialised with object specifics). For instance, `Room`s will always have `mapConnections` but never `edibility`.

We start with the module overview:

```haskell file=src/Yaifl/Objects/ObjectData.hs
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
  , ConnectionExplicitness(..)
  , Connection(..)
  , blankRoomData

  -- * Lenses
  , thingContainedBy, thingLit, thingWearable, thingDescribed, _Wearable
  , roomIsVisited, roomDarkness, roomMapConnections, roomContainingRegion, roomEnclosing

  , connectionExplicitness, connectionRoom
  ) where

import qualified Data.Map as Map
import Solitude 
import Yaifl.Common ( WMDirections, Entity, defaultVoidID )
import Yaifl.Properties.Enclosing ( Enclosing, blankEnclosing )

<<thing-data>>
<<connections>>
<<room-data>>
```

# Things

We have a bunch of fancy boolean flags. The special case is `ThingWearability`, where we wish to track both its ability to be worn and if it is worn then additionally who (maybe) is wearing it.

```haskell id=thing-data
-- | If a thing provides light outwards; A lamp is lit, but a closed box with a light inside is not.
data ThingLit = Lit | NotLit 
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | If a thing is wearable, and if so who (or what) is currently wearing it.
data ThingWearability = NotWearable | Wearable (Maybe Entity) 
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | If a thing appears in "You can also see..." paragraphs.
data ThingDescribed = Undescribed | Described 
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

data ThingData = ThingData
  { _thingContainedBy :: Entity
  , _thingLit :: ThingLit
  , _thingWearable :: ThingWearability
  , _thingDescribed :: ThingDescribed
  } deriving stock (Eq, Show, Read, Ord, Generic)

blankThingData :: ThingData
blankThingData = ThingData defaultVoidID NotLit NotWearable Described

makeLenses ''ThingData
makePrisms ''ThingWearability
```
# Rooms

## Connections

We formalise connections between rooms somewhat. `Inform7` has an (implicit) notion of connections, in that it will make the reverse mapping relation if it doesn't disturb something you have explicitly made already. For instance:

```inform
The West Room is a room. The East Room is a room. The Problem Room is a room.
The East Room is east of The West Room. The Problem Room is west of The East Room.
```

This will put `The East Room` to the east of `The West Room` (explicitly) and `The West Room` to the west of `The East Room` (implicitly). Then we add `The Problem Room` to the west of `The East Room`, and as this is an explicit relation it overrides the implicit one. If we had instead tried to put `The Problem Room` to the east of `The West Room`, this would fail (can't override explicit connections). This will be covered more in some later section.

```haskell id=connections

data ConnectionExplicitness = Explicit | Implicit 
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

data Connection = Connection 
  { _connectionExplicitness :: ConnectionExplicitness
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
```

## RoomData

And now we put together a couple of spatial properties (connections and regions) with some useful properties: Some rooms are inherently lit and therefore don't need a light source, and we also track whether the player has visited it to provide short descriptions on a return.

```haskell id=room-data
-- | Whether a room has an intrinsic light-ness. This isn't equivalent to whether a
-- room is currently dark - for instance, a cave may have light (if the player has a
-- lantern) but the cave will be Dark.
data Darkness = Lighted | Dark 
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | Whether a room has been visited before or not.
data IsVisited = Visited | Unvisited 
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

newtype ContainingRegion = ContainingRegion
  { unRegion :: Maybe Entity
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, Generic)

data RoomData wm = RoomData
  { _roomIsVisited :: IsVisited
  , _roomDarkness :: Darkness
  , _roomMapConnections :: MapConnections wm
  , _roomContainingRegion :: ContainingRegion
  , _roomEnclosing :: Enclosing
  } deriving stock (Generic)

deriving stock instance (Ord (WMDirections wm)) => Ord (RoomData wm)
deriving stock instance (Read (WMDirections wm), Ord (WMDirections wm)) => Read (RoomData wm)
deriving stock instance (Show (WMDirections wm)) => Show (RoomData wm)
deriving stock instance (Eq (WMDirections wm)) => Eq (RoomData wm)

blankRoomData :: RoomData wm
blankRoomData = RoomData Unvisited Lighted (MapConnections Map.empty) (ContainingRegion Nothing) blankEnclosing


makeLenses ''RoomData
makeLenses ''Connection
```
