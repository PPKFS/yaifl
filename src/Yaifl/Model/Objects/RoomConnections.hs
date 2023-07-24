{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Model.Objects.RoomConnections
  ( isWestOf
  , isSouthOf
  , getMapConnection
  ) where

import qualified Data.Map as Map

import Solitude

import Yaifl.Model.Direction
import Yaifl.Model.Entity (HasID(..), Entity)
import Yaifl.Metadata (Metadata, whenConstructing)
import Yaifl.Model.Object(Room)
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.RoomData
import Yaifl.Model.Properties.TH (makeDirections)
import Yaifl.Model.WorldModel (WMDirection, WMSayable)
import Breadcrumbs
import Data.Text.Display

hasSpecificConnectionTo ::
  WMStdDirections wm
  => Maybe ConnectionExplicitness
  -> Room wm
  -> WMDirection wm
  -> Maybe Entity
hasSpecificConnectionTo mbExpl r dir = case getConnectionInDirection dir r of
    Just (Connection ex' e)
      | maybe True (ex' ==) mbExpl -> Just e
    _ -> Nothing

getMapConnection ::
  WMStdDirections wm
  => WMDirection wm
  -> Room wm
  -> Maybe Entity
getMapConnection dir o = _connectionDestination <$> getConnectionInDirection dir o

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
makeConnection expl dir r = connectionLens dir ?~ Connection expl (getID r)

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
isDirectionFromInternal mkRev dir r1' r2' = withoutMissingObjects (do
    let opp = opposite dir
    r1 <- refreshRoom r1'
    r2 <- refreshRoom r2'
    -- we log a warning if we're in construction and we are overriding an explicit connection
    -- apparently inform just doesn't let you do this, so...
    -- r1 is explicitly dir of r2; it is r2 we need to check
    -- r2 is implicitly (opposite dir) of r1.
    -- e.g. if r1 `isWestOf` r2, then r2 has an explicit west connection and r1 has an implicit east connection.
    whenConstructing (isJust $ hasSpecificConnectionTo (Just Explicit) r2 dir)
      -- TODO: this should be a nonblocking failure
      (addAnnotation $ "Overriding an explicitly set map direction of room " <> "")--show r1)
    modifyRoom r2 (makeConnection Explicit dir r1)
    --only make the reverse if we want to
    when mkRev $ do
      -- something weird is happening if we're overriding an implicit direction with another implicit direction
      -- but I think in general we don't bother setting an implicit one
      whenConstructing (isJust $ hasSpecificConnectionTo (Just Implicit) r2 opp)
        (addAnnotation $ "Not using an implicit direction to overwrite an implicitly set map direction of room " <> "") --show r1)
      -- and don't bother if there's any connection at all
      unless (isJust $ r1 ^? connectionLens opp) $ modifyRoom r1 (makeConnection Implicit dir r2)
    pass) (handleMissingObject "failed to make direction" ())

makeDirections True ["West", "South"]
