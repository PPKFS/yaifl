module Yaifl.Components.Room
    (
        makeRoom,
        makeRoom',
        RoomData(..)
    )
where

import           Yaifl.Common
import           Yaifl.Say
import           Yaifl.Prelude
import Yaifl.Components.Object
import Yaifl.Components.Enclosing
import qualified Data.Set as DS

data Darkness = Lighted | Dark deriving (Eq, Show)
data IsVisited = Visited | Unvisited deriving (Eq, Show)
type MapConnections = Store Entity
type ContainingRegion = Maybe Entity

data RoomData = RoomData
    {
        _isVisited :: IsVisited,
        _mapConnections :: MapConnections,
        _containingRegion :: ContainingRegion
    } deriving Show
makeLenses ''RoomData

roomComponent :: Proxy RoomData
roomComponent = Proxy :: (Proxy RoomData)

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