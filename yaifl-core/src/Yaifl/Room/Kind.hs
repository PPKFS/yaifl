{-|
Module      : Yaifl.Room.Kind
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Rooms represent environmental locations in the game world that contain objects,
provide spatial context, and define the game's geography.

This module defines the `Room` type and its associated data structures:

- `Room`: The core room type wrapping `Object` with room-specific data
- `RoomData`: Comprehensive properties for room behaviour and state
- `Connection`: Room-to-room connections with direction and explicitness (author-created vs implied)
- `MapConnections`: Direction-based connection mapping
- Functions for querying room properties and managing room state

See also:
- `Yaifl.Object.Kind` for the base Object type
- `Yaifl.Enclosing.Kind` for enclosing functionality
- `Yaifl.Tag` for the tagging system used by `tagRoomEntity`
- `Yaifl.Metadata` for game state management used by `updateFirstRoom`
-}

module Yaifl.Room.Kind
  ( -- * Connection types
    ConnectionExplicitness(..)
  , Connection(..)
  , MapConnections(..)

    -- * Room components
  , ContainingRegion(..)
  , Darkness(..)
  , RoomData(..)
  , IsVisited(..)
  , blankRoomData

    -- * Room type
  , Room(..)

    -- * Entity tagging
  , tagRoomEntity

    -- * Default IDs
  , voidID

    -- * Room queries and utilities
  , roomIsVisited
  , roomIsNotVisited
  , roomIsLighted
  , roomConnections
  , roomRegion
  , roomEnclosing
  , isVoid
  , updateFirstRoom

  ) where

import Yaifl.Prelude

import GHC.Records
import Yaifl.Entity
import Yaifl.Enclosing.Kind
import Yaifl.Object.Kind
import Yaifl.Tag
import Yaifl.WorldModel
import qualified Data.Map.Strict as Map
import Yaifl.Metadata

-- | Whether a connection was made by the user or simply assumed (we can override assumed connections but
-- error on overriding explicitly made connections).
data ConnectionExplicitness = Explicit | Implicit
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | Connection between rooms with direction and optional door.
data Connection wm = Connection
  { explicitness :: ConnectionExplicitness -- ^ Explicit (author-created) or implicit
  , otherSide :: RoomEntity -- ^ Connected room entity
  , doorThrough :: Maybe DoorEntity -- ^ Optional blocking door
  , direction :: WMDirection wm -- ^ Connection direction
  } deriving stock (Generic)

deriving stock instance (Eq (WMDirection wm)) => Eq (Connection wm)
deriving stock instance (Show (WMDirection wm)) => Show (Connection wm)
deriving stock instance (Read (WMDirection wm)) => Read (Connection wm)
deriving stock instance (Ord (WMDirection wm)) => Ord (Connection wm)


-- | Room connections mapped by direction.
newtype MapConnections wm = MapConnections
  { unMapConnections :: Map.Map (WMDirection wm) (Connection wm) -- ^ Direction-to-connection mapping
  }

deriving newtype instance (Generic (Map (WMDirection wm) (Connection wm))) => Generic (MapConnections wm)
deriving newtype instance (Ord (WMDirection wm)) => Ord (MapConnections wm)
deriving newtype instance (Read (WMDirection wm), Ord (WMDirection wm)) => Read (MapConnections wm)
deriving newtype instance (Show (WMDirection wm)) => Show (MapConnections wm)
deriving stock instance (Eq (WMDirection wm)) => Eq (MapConnections wm)

-- | Whether a room has an intrinsic light-ness. This isn't equivalent to whether a
-- room is currently dark - for instance, a cave may have light (if the player has a
-- lantern) but the cave will be Dark.
data Darkness = Lighted | Dark
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | Whether a room has been visited before or not.
data IsVisited = Visited | Unvisited
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | The region in which a room is located.
newtype ContainingRegion = ContainingRegion
  { unRegion :: Maybe Entity
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, Generic)

-- | Core room properties and state.
data RoomData wm = RoomData
  { isVisited :: IsVisited -- ^ Visitation status
  , darkness :: Darkness -- ^ Intrinsic light level
  , mapConnections :: MapConnections wm -- ^ Room connections by direction
  , containingRegion :: ContainingRegion -- ^ Parent region
  , enclosing :: Enclosing -- ^ Containment data
  , roomData :: WMRoomData wm -- ^ World-model-specific data
  } deriving stock (Generic)

deriving stock instance (Ord (WMDirection wm), Ord (WMRoomData wm)) => Ord (RoomData wm)
deriving stock instance (Read (WMDirection wm), Ord (WMDirection wm), Read (WMRoomData wm)) => Read (RoomData wm)
deriving stock instance (Show (WMDirection wm), Show (WMRoomData wm)) => Show (RoomData wm)
deriving stock instance (Eq (WMDirection wm), Eq (WMRoomData wm)) => Eq (RoomData wm)

-- | A default for a room.
blankRoomData :: Pointed (WMRoomData wm) => RoomData wm
blankRoomData = RoomData Unvisited Lighted (MapConnections Map.empty) (ContainingRegion Nothing) blankEnclosing identityElement

makeFieldLabelsNoPrefix ''RoomData
makeFieldLabelsNoPrefix ''Connection

-- | A room object with room-specific data and behaviour.
--
-- Wraps an `Object` with `RoomData`, providing access to room properties
-- such as connections, darkness, visitation state, and regional containment.
-- Maintains compatibility with the object system via `HasEntity` and `IsObject` instances.
newtype Room wm = Room (Object wm (RoomData wm) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)

instance HasField x (Object wm (RoomData wm) (WMObjSpecifics wm)) a  => HasField x (Room wm) a where
  getField (Room o) = getField @x o

instance Display (Room wm) where
  displayBuilder = const "room"

instance HasEntity (Room wm) where
  getEntity (Room a) = objectId a

-- | The Void room ID used as a default placement location.
-- The Void (Entity -1) serves as a fallback to avoid `Maybe` locations
-- when objects need a default container. Objects in The Void are not
-- considered part of the active game world and may be removed at any time.
voidID :: TaggedEntity RoomTag
voidID = unsafeTagEntity $ Entity (-1)

instance Taggable (Room wm) EnclosingTag
instance Taggable (Room wm) RoomTag

-- | Tag a room with its entity for type-safe references.
-- Uses the room's object ID to create a tagged entity reference.
tagRoomEntity ::
  Room wm
  -> TaggedEntity RoomTag
tagRoomEntity r = tagEntity r (r ^. #objectId)

instance IsObject (Room wm) where
  isThing = const False

-- | Check if a room has been visited.
roomIsVisited ::
  Room wm
  -> Bool
roomIsVisited = (== Visited) . view (#objectData % #isVisited)

-- | Check if a room has not been visited.
roomIsNotVisited ::
  Room wm
  -> Bool
roomIsNotVisited = not . roomIsVisited

-- | Check if a room is currently lighted.
roomIsLighted ::
  Room wm
  -> Bool
roomIsLighted = (== Lighted) . view (#objectData % #darkness)

-- | Check if an object is the void ID (default placement location).
isVoid ::
  HasEntity a
  => a
  -> Bool
isVoid = (unTagEntity voidID ==) . getEntity

-- | Update the first room in metadata to the given room.
-- This sets the default starting location for the player.
updateFirstRoom ::
  State Metadata :> es
  => Room wm
  -> Eff es ()
updateFirstRoom e = #firstRoom .= tagRoomEntity e

-- | Get the connections from a room to other rooms.
roomConnections ::
  Room wm
  -> MapConnections wm
roomConnections = view (#objectData % #mapConnections)

-- | Get the region containing a room.
roomRegion ::
  Room wm
  -> ContainingRegion
roomRegion = view (#objectData % #containingRegion)

-- | Get the enclosing data for a room.
roomEnclosing ::
  Room wm
  -> Enclosing
roomEnclosing = view (#objectData % #enclosing)