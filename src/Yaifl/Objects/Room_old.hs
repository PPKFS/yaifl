module Yaifl.Components.Room
  ( RoomData (..),
    RoomObject (..),
    Darkness (..),
    IsVisited (..),
    roomObject,
    rooms,
    HasRoom,
    makeRoom,
    updateFirstRoom,
    roomObjData,
    darkness,
    roomEnclosing,
    deleteRoom,
    room,
  )
where

import Colog hiding (Lens')
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as DS
import Yaifl.Common
import Yaifl.Components.Enclosing
import Yaifl.Components.Object
import Yaifl.Prelude
import Yaifl.Utils

data Darkness = Lighted | Dark deriving (Eq, Show)

data IsVisited = Visited | Unvisited deriving (Eq, Show)

type MapConnections = Store Entity

type ContainingRegion = Maybe Entity

data RoomData = RoomData
  { _isVisited :: IsVisited,
    _darkness :: Darkness,
    _mapConnections :: MapConnections,
    _containingRegion :: ContainingRegion
  }
  deriving (Show)

data RoomObject w = RoomObject
  { _roomObject :: Object w,
    _roomObjData :: RoomData,
    _roomEnclosing :: Enclosing
  }
  deriving (Show)

makeClassy ''RoomData
makeLenses ''RoomObject

instance HasObject (RoomObject w) w where
  object = roomObject

instance HasRoom w => HasStore w (RoomObject w) where
  store = rooms

instance Monad m => ThereIs w (RoomObject w) m where
  defaultObject n d e = return $ RoomObject (Object n d e "room") (RoomData Visited Lighted IM.empty Nothing) (Enclosing DS.empty Nothing)

makeRoom :: forall w m a. (HasRoom w, HasStore w (Physical w), WithGameData w m) => Text -> Description w -> State (RoomObject w) a -> m (RoomObject w)
makeRoom = thereIs @(RoomObject w)

instance HasRoom w => Deletable w (RoomObject w) where
  deleteObject e = do
    deleteComponent @(Object w) e
    deleteComponent @RoomData e
    deleteComponent @Enclosing e
    pass

type HasRoom w = (HasObjectStore w, HasStore w RoomData, HasStore w Enclosing)

deleteRoom :: forall w m. (WithGameData w m, HasRoom w) => Entity -> m ()
deleteRoom = deleteObject @w @(RoomObject w)

rooms :: HasRoom w => Lens' w (Store (RoomObject w))
rooms = storeLens3 RoomObject _roomObject _roomObjData _roomEnclosing

room :: HasRoom w => Entity -> Lens' w (Maybe (RoomObject w))
room k = rooms . at k

--UNSAFE.
updateFirstRoom :: forall w m. WithGameData w m => HasRoom w => m Entity
updateFirstRoom = do
  ks <- uses (gameWorld . store @w @(RoomObject w)) (nonEmpty . IM.keys)
  maybe
    ( do
        logError "No rooms detected, so couldn't find first room."
        return (-10)
    )
    ( \t -> do
        let v = minimumNE t
        firstRoom ?= v
        getObject' v >>= (\o' -> logInfo $ "Added the first room " <> showObjDebug o')
        return v
    )
    ks
