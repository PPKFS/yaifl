{-# LANGUAGE TemplateHaskell #-}

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
  ) where

import qualified Data.Map as Map

import Solitude hiding (Down)

import Yaifl.Model.Direction
import Yaifl.Model.Entity (HasID(..), Entity)
import Yaifl.Metadata (Metadata, whenConstructing)
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.RoomData
import Yaifl.Model.Properties.TH (makeDirections)
import Yaifl.Model.WorldModel (WMDirection, WMSayable)
import Breadcrumbs
import Data.Text.Display
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Properties.Door
import Yaifl.Model.Properties.Has

getAllConnections ::
  Room wm
  -> Map (WMDirection wm) Connection
getAllConnections r = r ^. #objectData % #mapConnections % coerced

hasSpecificConnectionTo ::
  WMStdDirections wm
  => Maybe ConnectionExplicitness
  -> Room wm
  -> WMDirection wm
  -> Maybe Entity
hasSpecificConnectionTo mbExpl r dir = case getConnectionInDirection dir r of
    Just (Connection ex' e _d)
      | maybe True (ex' ==) mbExpl -> Just e
    _ -> Nothing

getMapConnection ::
  WMStdDirections wm
  => WMDirection wm
  -> Room wm
  -> Maybe Entity
getMapConnection dir o = destination <$> getConnectionInDirection dir o

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
makeConnection expl dir r = connectionLens dir ?~ Connection expl (getID r) Nothing

addDirectionFrom ::
  ObjectQuery wm es
  => State Metadata :> es
  => Breadcrumbs :> es
  => WMStdDirections wm
  => Display (WMSayable wm)
  => WMDirection wm
  -> Room wm
  -> Room wm
  -> Eff es ()
addDirectionFrom = isDirectionFromInternal True

addDirectionFromOneWay ::
  ObjectQuery wm es
  => State Metadata :> es
  => Breadcrumbs :> es
  => WMStdDirections wm
  => Display (WMSayable wm)
  => WMDirection wm
  -> Room wm
  -> Room wm
  -> Eff es ()
addDirectionFromOneWay = isDirectionFromInternal False

-- | the ordering here is that r2' `isSouthOf` (for example) r1 means
-- the connection to be made explicitly is from r1 (south) -> r2
-- then the implicit reverse connection is r2 (opposite south) -> r1
isDirectionFromInternal ::
  State Metadata :> es
  => Breadcrumbs :> es
  => WMStdDirections wm
  => ObjectQuery wm es
  => Display (WMSayable wm)
  => Bool
  -> WMDirection wm
  -> Room wm
  -> Room wm
  -> Eff es ()
isDirectionFromInternal mkRev dir r2' r1' = withoutMissingObjects (do
    let opp = opposite dir
    r1 <- refreshRoom r1'
    r2 <- refreshRoom r2'
    -- we log a warning if we're in construction and we are overriding an explicit connection
    -- apparently inform just doesn't let you do this, so...
    -- r2 is explicitly dir of r1; it is r1 we need to check
    -- r2 is implicitly (opposite dir) of r1.
    -- e.g. if r1 `isWestOf` r2, then r2 has an explicit west connection and r1 has an implicit east connection.
    whenConstructing (isJust $ hasSpecificConnectionTo (Just Explicit) r1 dir)
      -- TODO: this should be a nonblocking failure
      (addAnnotation $ "Overriding an explicitly set map direction of room " <> "")--show r1)
    modifyRoom r1 (makeConnection Explicit dir r2)
    --only make the reverse if we want to
    when mkRev $ do
      -- something weird is happening if we're overriding an implicit direction with another implicit direction
      -- but I think in general we don't bother setting an implicit one\
      whenConstructing (isJust $ hasSpecificConnectionTo (Just Implicit) r2 opp)
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
    pass) (handleMissingObject "failed to make direction" ())

makeDirections True ["West", "South", "North", "East", "In", "Out", "Up", "Down"]

isInsideFrom ::
  Breadcrumbs :> es
  => ObjectQuery wm es
  => State Metadata :> es
  => Display (WMSayable wm)
  => WMStdDirections wm
  => Room wm
  -> Room wm
  -> Eff es ()
isInsideFrom = isInOf

isOutsideFrom ::
  Breadcrumbs :> es
  => ObjectQuery wm es
  => State Metadata :> es
  => Display (WMSayable wm)
  => WMStdDirections wm
  => Room wm
  -> Room wm
  -> Eff es ()
isOutsideFrom = isOutOf

isAbove ::
  Breadcrumbs :> es
  => ObjectQuery wm es
  => State Metadata :> es
  => Display (WMSayable wm)
  => WMStdDirections wm
  => Room wm
  -> Room wm
  -> Eff es ()
isAbove = isUpOf

isBelow ::
  Breadcrumbs :> es
  => ObjectQuery wm es
  => State Metadata :> es
  => Display (WMSayable wm)
  => WMStdDirections wm
  => Room wm
  -> Room wm
  -> Eff es ()
isBelow = isDownOf

addDoorToConnection ::
  NoMissingObjects wm es
  => WMHasProperty wm DoorSpecifics
  => DoorLike wm d
  => d
  -> (Room wm, WMDirection wm)
  -> (Room wm, WMDirection wm)
  -> Eff es ()
addDoorToConnection d (front, frontDir) (back, backDir) = do
  mbDs <- getDoorSpecificsMaybe d
  case mbDs of
    Nothing -> error $ "Tried to add a door, except it wasn't a door " <> show (getID d)
    Just ds -> do
      -- so now we need to assign the sides of the door if they aren't already
      pass
