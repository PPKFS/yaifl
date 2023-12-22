

module Yaifl.Model.Objects.RoomConnections
  ( isWestOf
  , isSouthOf
  , isEastOf
  , isNorthOf
  , isInsideFrom
  , isOutsideFrom
  , isAbove
  , isBelow
  , getMapConnection
  , getAllConnections
  , addDoorToConnection
  ) where

import qualified Data.Map as Map

import Solitude hiding (Down)

import Yaifl.Model.Direction
import Yaifl.Model.Objects.Entity
import Yaifl.Metadata ( whenConstructing, noteError )
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.RoomData
import Yaifl.Model.Properties.TH (makeDirections)
import Yaifl.Model.WorldModel ( WMDirection )
import Breadcrumbs
import Data.Text.Display
import Yaifl.Model.Objects.Effects

getAllConnections ::
  Room wm
  -> Map (WMDirection wm) Connection
getAllConnections r = r ^. #objectData % #mapConnections % coerced

connectionInDirection ::
  WMStdDirections wm
  => Maybe ConnectionExplicitness
  -> Room wm
  -> WMDirection wm
  -> Maybe RoomEntity
connectionInDirection mbExpl r dir = case getConnectionInDirection dir r of
    Just c
      | maybe True ((c ^. #explicitness) ==) mbExpl -> Just (c ^. #otherSide)
    _ -> Nothing

getMapConnection ::
  WMStdDirections wm
  => WMDirection wm
  -> Room wm
  -> Maybe RoomEntity
getMapConnection dir o = view #otherSide <$> getConnectionInDirection dir o

getConnectionInDirection ::
  WMStdDirections wm
  => WMDirection wm
  -> Room wm
  -> Maybe Connection
getConnectionInDirection dir = preview (connectionLens dir % _Just)

connectionLens ::
  forall wm.
  WMStdDirections wm
  => WMDirection wm
  -> Lens' (Room wm) (Maybe Connection)
connectionLens dir = (#objectData :: Lens' (Room wm) (RoomData wm)) % #mapConnections % coercedTo @(Map.Map (WMDirection wm) Connection ) % at dir

makeConnection ::
  WMStdDirections wm
  => ConnectionExplicitness
  -> WMDirection wm
  -> Room wm
  -> (Room wm -> Room wm)
makeConnection expl dir r = connectionLens dir ?~ Connection expl (tagRoom r) Nothing

addDirectionFrom ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => WMDirection wm
  -> Room wm
  -> Room wm
  -> Eff es ()
addDirectionFrom = isDirectionFromInternal True

addDirectionFromOneWay ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => WMDirection wm
  -> Room wm
  -> Room wm
  -> Eff es ()
addDirectionFromOneWay = isDirectionFromInternal False

-- | the ordering here is that r2' `isSouthOf` (for example) r1 means
-- the connection to be made explicitly is from r1 (south) -> r2
-- then the implicit reverse connection is r2 (opposite south) -> r1
isDirectionFromInternal ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => Bool
  -> WMDirection wm
  -> Room wm
  -> Room wm
  -> Eff es ()
isDirectionFromInternal mkRev dir r2' r1' = do
    let opp = opposite dir
    r1 <- refreshRoom r1'
    r2 <- refreshRoom r2'
    -- we log a warning if we're in construction and we are overriding an explicit connection
    -- apparently inform just doesn't let you do this, so...
    -- r2 is explicitly dir of r1; it is r1 we need to check
    -- r2 is implicitly (opposite dir) of r1.
    -- e.g. if r1 `isWestOf` r2, then r2 has an explicit west connection and r1 has an implicit east connection.
    whenConstructing (isJust $ connectionInDirection (Just Explicit) r1 dir)
      -- TODO: this should be a nonblocking failure
      (addAnnotation $ "Overriding an explicitly set map direction of room " <> "")--show r1)
    modifyRoom r1 (makeConnection Explicit dir r2)
    --only make the reverse if we want to
    when mkRev $ do
      -- something weird is happening if we're overriding an implicit direction with another implicit direction
      -- but I think in general we don't bother setting an implicit one
      whenConstructing (isJust $ connectionInDirection (Just Implicit) r2 opp)
        (addAnnotation $ "Not using an implicit direction to overwrite an implicitly set map direction of room " <> "") --show r1)
      -- and don't bother if there's any connection at all
      if isJust $ r2 ^? connectionLens opp
        then do
          modifyRoom r2 (makeConnection Implicit opp r1)
          addAnnotation $ "made implicit connection from " <> display (view #name r2) <> " going " <> show opp <> " to " <> display (view #name r1)
        else
          addAnnotation $ "did not make implicit connection from " <> display (view #name r2) <> " going " <> show opp <> " to " <> display (view #name r1)
            <> " because it's already made."
    addAnnotation $ "made connection from " <> display (view #name r1) <> " going " <> show dir <> " to " <> display (view #name r2)

makeDirections True ["West", "South", "North", "East", "In", "Out", "Up", "Down"]

isInsideFrom ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => Room wm
  -> Room wm
  -> Eff es ()
isInsideFrom = isInOf

isOutsideFrom ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => Room wm
  -> Room wm
  -> Eff es ()
isOutsideFrom = isOutOf

isAbove ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => Room wm
  -> Room wm
  -> Eff es ()
isAbove = isUpOf

isBelow ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => Room wm
  -> Room wm
  -> Eff es ()
isBelow = isDownOf

addDoorToConnection ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => DoorEntity
  -> (Room wm, WMDirection wm)
  -> (Room wm, WMDirection wm)
  -> Eff es ()
addDoorToConnection d (front, frontDir) (back, backDir) = do
  modifyAndVerifyConnection front frontDir back (#doorThrough ?~ d)
  modifyAndVerifyConnection back backDir front (#doorThrough ?~ d)

modifyAndVerifyConnection ::
  forall wm es.
  WMStdDirections wm
  => NoMissingObjects wm es
  => Room wm
  -> WMDirection wm
  -> Room wm
  -> (Connection -> Connection)
  -> Eff es ()
modifyAndVerifyConnection fromRoom' fromDir dest f = do
  fromRoom <- refreshRoom fromRoom'
  if connectionInDirection Nothing fromRoom fromDir == Just (tagRoom dest)
  then modifyRoom @wm fromRoom (connectionLens fromDir % _Just %~ f)
  else noteError (const ()) ("Tried to add a connection to the room " <> display fromRoom <> " but it had no connection in direction "
    <> display fromDir <> ". Directions that do exist are " <> show (getAllConnections fromRoom))
