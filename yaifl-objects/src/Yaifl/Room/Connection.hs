module Yaifl.Room.Connection
  ( getMapConnection
  , getConnection
  , getAllConnections
  , getConnectionViaDoor
  , connectionInDirection
  , makeConnection
  , connectionLens
  ) where

import qualified Data.Map as Map

import Yaifl.Direction.Kind
import Yaifl.Object.Kind
import Yaifl.Entity
import Yaifl.Room.Kind
import Yaifl.WorldModel ( WMDirection )

import qualified Data.Map as M
import Yaifl.Prelude

connectionLens ::
  forall wm.
  WMStdDirections wm
  => WMDirection wm
  -> Lens' (Room wm) (Maybe (Connection wm))
connectionLens dir = (#objectData :: Lens' (Room wm) (RoomData wm)) % #mapConnections % coercedTo @(Map.Map (WMDirection wm) (Connection wm)) % at dir

getAllConnections ::
  Room wm
  -> Map (WMDirection wm) (Connection wm)
getAllConnections r = r ^. #objectData % #mapConnections % coerced

connectionInDirection ::
  WMStdDirections wm
  => Maybe ConnectionExplicitness
  -> Room wm
  -> WMDirection wm
  -> Maybe RoomEntity
connectionInDirection mbExpl r dir = case getConnection dir r of
    Just (_, c)
      | maybe True ((c ^. #explicitness) ==) mbExpl -> Just (c ^. #otherSide)
    _ -> Nothing

getMapConnection ::
  WMStdDirections wm
  => WMDirection wm
  -> Room wm
  -> Maybe RoomEntity
getMapConnection dir o = fst <$> getConnection dir o

getConnection ::
  WMStdDirections wm
  => WMDirection wm
  -> Room wm
  -> Maybe (RoomEntity, Connection wm)
getConnection dir = (view #otherSide &&& id) <$$> preview (connectionLens dir % _Just)

getConnectionViaDoor ::
  DoorEntity
  -> Room wm
  -> Maybe (RoomEntity, Connection wm)
getConnectionViaDoor door = ((view #otherSide &&& id) <$$> find (\c -> c ^. #doorThrough == Just door)) . M.elems . getAllConnections

makeConnection ::
  WMStdDirections wm
  => ConnectionExplicitness
  -> WMDirection wm
  -> Room wm
  -> (Room wm -> Room wm)
makeConnection expl dir r = connectionLens dir ?~ Connection expl (tagRoomEntity r) Nothing dir