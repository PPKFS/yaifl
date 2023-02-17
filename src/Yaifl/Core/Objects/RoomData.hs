{-|
Module      : Yaifl.Core.Objects.RoomData
Description : Properties that define a `Yaifl.Core.Object.Room`.
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com
-}

{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Core.Objects.RoomData (
  -- * Room data
  -- ** Connections
  ConnectionExplicitness(..)
  , Connection(..)
  , MapConnections(..)
  -- ** Rooms
  , ContainingRegion(..)
  , Darkness(..)
  , RoomData(..)
  , IsVisited(..)
  , blankRoomData
  ) where

import Solitude
import qualified Data.Map as Map

import Yaifl.Core.Entity ( Entity )
import Yaifl.Core.Properties.Enclosing ( Enclosing, blankEnclosing )
import Yaifl.Core.WorldModel ( WMDirection )

-- | Whether a connection was made by the user or simply assumed (we can override assumed connections but
-- error on overriding explicitly made connections).
data ConnectionExplicitness = Explicit | Implicit
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | A connection from one room (or door) to another.
data Connection = Connection
  { _connectionExplicitness :: ConnectionExplicitness
  , _connectionDestination :: Entity -- ^ The 'other side' of this connection.
  } deriving stock (Eq, Show, Read, Ord, Generic)

-- | The connections from one room to another, stored by direction ID.
newtype MapConnections wm = MapConnections
  { unMapConnections :: Map.Map (WMDirection wm) Connection
  }

deriving newtype instance (Generic (Map (WMDirection wm) Connection)) => Generic (MapConnections wm)
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

-- | The properties that make up a room.
data RoomData wm = RoomData
  { isVisited :: IsVisited
  , darkness :: Darkness
  , mapConnections :: MapConnections wm
  , containingRegion :: ContainingRegion
  , enclosing :: Enclosing
  } deriving stock (Generic)

deriving stock instance (Ord (WMDirection wm)) => Ord (RoomData wm)
deriving stock instance (Read (WMDirection wm), Ord (WMDirection wm)) => Read (RoomData wm)
deriving stock instance (Show (WMDirection wm)) => Show (RoomData wm)
deriving stock instance (Eq (WMDirection wm)) => Eq (RoomData wm)

-- | A default for a room.
blankRoomData :: RoomData wm
blankRoomData = RoomData Unvisited Lighted (MapConnections Map.empty) (ContainingRegion Nothing) blankEnclosing
