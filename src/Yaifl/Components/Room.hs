module Yaifl.Components.Room
    (
        RoomData(..)
      , RoomObject(..)
      , Darkness(..)
      , IsVisited(..)
      , roomObject
      , rooms
    )
where

import           Yaifl.Common
import           Yaifl.Say
import           Yaifl.Prelude
import Yaifl.Components.Object
import Yaifl.Components.Enclosing

import qualified Data.Set as DS
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Merge.Strict as IMerge
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Merge.Strict as MapMerge

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
{-
sa1 :: (HasStore w Object, HasStore w RoomData, HasStore w Enclosing) => w -> Store _
sa1 w = IMerge.merge IMerge.dropMissing IMerge.dropMissing (IMerge.zipWithMatched (\_ x y -> x y)) (sa2 w) (getStore w) --Map.intersectionWith (.) 

sa2 :: (HasStore w Object, HasStore w RoomData) => w -> Store _
sa2 w = IMerge.merge IMerge.dropMissing IMerge.dropMissing (IMerge.zipWithMatched (const RoomObject)) (getStore w) (getStore w)

-}

rooms :: (HasStore w Object, HasStore w RoomData, HasStore w Enclosing) => Lens' w (Store RoomObject)
rooms = lens (intersectStore3 RoomObject) (setStore3 _roomObject _roomObjData _roomEnclosing)
{-
room :: Text -> Traversal' TestLens RoomObject
interim k = (((foo1 . ix k) `fanoutTraversal` (foo2 . ix k)) `fanoutTraversal` (foo3 . ix k)) . interimIso
  where
    interimIso = iso (\((a,b),c) -> Interim a b c) (\(Interim a b c) -> ((a,b),c))
-}
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