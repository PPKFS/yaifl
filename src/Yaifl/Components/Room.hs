module Yaifl.Components.Room
    (
        RoomData(..)
      , RoomObject(..)
      , Darkness(..)
      , IsVisited(..)
      , roomObject
      , rooms
      , HasRoom
      , updateFirstRoom
    )
where

import           Yaifl.Common
import           Yaifl.Prelude
import Yaifl.Components.Object
import Yaifl.Components.Enclosing

import qualified Data.Set as DS
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Merge.Strict as IMerge
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Merge.Strict as MapMerge
import Colog hiding (Lens')
import Yaifl.Utils

data Darkness = Lighted | Dark deriving (Eq, Show)
data IsVisited = Visited | Unvisited deriving (Eq, Show)
type MapConnections = Store Entity
type ContainingRegion = Maybe Entity

data RoomData = RoomData
    {
        _isVisited :: IsVisited,
        _darkness :: Darkness,
        _mapConnections :: MapConnections,
        _containingRegion :: ContainingRegion
    } deriving Show

data RoomObject = RoomObject
    {
        _roomObject :: Object 
      , _roomObjData :: RoomData
      , _roomEnclosing :: Enclosing
    } deriving Show

            
makeClassy ''RoomData
makeLenses ''RoomObject

instance HasObject RoomObject where
    object = roomObject

instance HasRoom w => HasStore w RoomObject where
    store = rooms

instance ThereIs RoomObject where
    defaultObject e = RoomObject (blankObject e "room") (RoomData Visited Lighted IM.empty Nothing) (Enclosing DS.empty)

instance HasRoom w => Deletable w RoomObject where
    deleteObject e = do
        deleteComponent @Object e
        deleteComponent @RoomData e
        deleteComponent @Enclosing e
        pass

type HasRoom w = (HasStore w Object, HasStore w RoomData, HasStore w Enclosing)
deleteRoom :: forall w m. (Monad m, HasRoom w) => Entity -> World w m ()
deleteRoom = deleteObject @w @RoomObject


rooms :: HasRoom w => Lens' w (Store RoomObject)
rooms = storeLens3 RoomObject _roomObject _roomObjData _roomEnclosing

room :: HasRoom w => Entity -> Lens' w (Maybe RoomObject)
room k = rooms . at k

--UNSAFE.
updateFirstRoom :: forall w m. Monad m => HasRoom w => World w m Entity
updateFirstRoom = do
    ks <- uses (gameWorld . store @w @RoomObject) (nonEmpty . IM.keys)
    maybe (do
        logError "No rooms detected, so couldn't find first room."
        return (-10))  (\t -> do
        let v = minimumNE t
        o <- getComponent @Object v
        firstRoom ?= v
        logInfo $ "Added the first room " <> showMaybeObjDebug o
        return v) ks
{-
makeRoom :: HasWorld w '[Enclosing, RoomData] r => Text -> Description -> Sem r Entity
makeRoom n d = do
    addContext "RoomConstruction" Nothing
    e <- makeObject n d "room"
    addComponent e (RoomData Unvisited emptyStore Nothing)
    addComponent e (Enclosing DS.empty)
    removeContext
    return e

makeRoom' :: HasWorld w '[Enclosing, RoomData] r => Text -> Sem r Entity
makeRoom' n = makeRoom n $ "It's " <> PlainDescription n <> "."

-}