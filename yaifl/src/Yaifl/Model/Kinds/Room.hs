module Yaifl.Model.Kinds.Room
  ( ConnectionExplicitness(..)
  , Connection(..)
  , MapConnections(..)
  -- ** Rooms
  , ContainingRegion(..)
  , Darkness(..)
  , RoomData(..)
  , IsVisited(..)
  , blankRoomData
  , Room(..)
  , tagRoom
  , voidID
  , isNotVisited
  , roomIsLighted

  ) where

import Yaifl.Prelude

import GHC.Records
import Yaifl.Model.Entity
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Tag
import Yaifl.Model.WorldModel
import qualified Data.Map.Strict as Map

-- | Whether a connection was made by the user or simply assumed (we can override assumed connections but
-- error on overriding explicitly made connections).
data ConnectionExplicitness = Explicit | Implicit
  deriving stock (Eq, Show, Read, Enum, Ord, Generic)

-- | A connection from one room to another.
data Connection wm = Connection
  { explicitness :: ConnectionExplicitness
  , otherSide :: RoomEntity
  , doorThrough :: Maybe DoorEntity
  , direction :: WMDirection wm
  } deriving stock (Generic)

deriving stock instance (Eq (WMDirection wm)) => Eq (Connection wm)
deriving stock instance (Show (WMDirection wm)) => Show (Connection wm)
deriving stock instance (Read (WMDirection wm)) => Read (Connection wm)
deriving stock instance (Ord (WMDirection wm)) => Ord (Connection wm)


-- | The connections from one room to another, stored by direction.
newtype MapConnections wm = MapConnections
  { unMapConnections :: Map.Map (WMDirection wm) (Connection wm)
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

makeFieldLabelsNoPrefix ''RoomData
makeFieldLabelsNoPrefix ''Connection

-- | An `Object` with `RoomData`.
newtype Room wm = Room (Object wm (RoomData wm) (WMObjSpecifics wm))
  deriving newtype (Eq, Ord, Generic)

instance HasField x (Object wm (RoomData wm) (WMObjSpecifics wm)) a  => HasField x (Room wm) a where
  getField (Room o) = getField @x o

instance Display (Room wm) where
  displayBuilder = const "room"

instance HasID (Room wm) where
  getID (Room a) = objectId a

-- | A place where new `Yaifl.Model.Objects.Thing`s are placed by default, to avoid having locations be `Maybe`.
voidID :: TaggedEntity RoomTag
voidID = unsafeTagEntity $ Entity (-1)

instance Taggable (Room wm) EnclosingTag
instance Taggable (Room wm) RoomTag

-- | Tag a room entity.
tagRoom ::
  Room wm
  -> TaggedEntity RoomTag
tagRoom r = tagEntity r (r ^. #objectId)

instance IsObject (Room wm) where
  isThing = const False

isNotVisited ::
  RoomData wm
  -> Bool
isNotVisited = (/= Visited) . isVisited

roomIsLighted ::
  Room wm
  -> Bool
roomIsLighted = (== Lighted) . view (#objectData % #darkness)