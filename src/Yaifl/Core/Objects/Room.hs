-- ~\~ language=Haskell filename=src/Yaifl/Core/Objects/Room.hs
-- ~\~ begin <<lit/worldmodel/connections.md|src/Yaifl/Core/Objects/Room.hs>>[0] project://lit/worldmodel/connections.md:4

{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Objects.Room
  ( isWestOf
  , getMapConnection
  ) where

import Cleff.State ( State )
import qualified Data.Map as Map

import Yaifl.Core.Common ( Entity, HasID(getID), Metadata, WMDirections, whenConstructing )
import Yaifl.Core.Directions
import Yaifl.Core.Logger ( warn, Log )
import Yaifl.Core.Objects.Object ( objData, Room )
import Yaifl.Core.Objects.ObjectData ( MapConnections(MapConnections), Connection(..), ConnectionExplicitness(..), roomMapConnections )
import Yaifl.Core.Objects.Query
import Yaifl.Core.Properties.TH

hasSpecificConnectionTo ::
  WithDirections wm
  => Maybe ConnectionExplicitness
  -> Room wm
  -> WMDirections wm
  -> Maybe Entity
hasSpecificConnectionTo mbExpl r dir = let v = getConnectionInDirection dir r in
  case v of
    Just (Connection ex' e)
      | maybe True (ex' ==) mbExpl -> Just e
    _ -> Nothing

getMapConnection ::
  WithDirections wm
  => WMDirections wm
  -> Room wm
  -> Maybe Entity
getMapConnection dir o = _connectionRoom <$> getConnectionInDirection dir o

getConnectionInDirection ::
  WithDirections wm
  => WMDirections wm
  -> Room wm
  -> Maybe Connection
getConnectionInDirection dir = preview (connectionLens dir % _Just)

connectionLens ::
  forall wm.
  WithDirections wm
  => WMDirections wm
  -> Lens' (Room wm) (Maybe Connection)
connectionLens dir = objData % roomMapConnections % coercedTo @(Map.Map (WMDirections wm) Connection ) % at dir

makeConnection ::
  WithDirections wm
  => ConnectionExplicitness
  -> WMDirections wm
  -> Room wm
  -> (Room wm -> Room wm)
makeConnection expl dir r = connectionLens dir ?~ Connection expl (getID r)

addDirectionFrom ::
  ObjectQuery wm es
  => State (Metadata) :> es
  => Log :> es
  => WithDirections wm
  => WMDirections wm
  -> Room wm
  -> Room wm
  -> Eff es ()
addDirectionFrom = isDirectionFromInternal True

addDirectionFromOneWay ::
  State (Metadata) :> es
  => Log :> es
  => ObjectQuery wm es
  => WithDirections wm
  => WMDirections wm
  -> Room wm
  -> Room wm
  -> Eff es ()
addDirectionFromOneWay = isDirectionFromInternal False

isDirectionFromInternal ::
  State (Metadata) :> es
  => Log :> es
  => ObjectQuery wm es
  => WithDirections wm
  => Bool
  -> WMDirections wm
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
      (warn $ "Overriding an explicitly set map direction of room " <> "")--show r1)
    modifyRoom r2 (makeConnection Explicit dir r1)
    --only make the reverse if we want to
    when mkRev $ do
      -- something weird is happening if we're overriding an implicit direction with another implicit direction
      -- but I think in general we don't bother setting an implicit one
      whenConstructing (isJust $ hasSpecificConnectionTo (Just Implicit) r2 opp)
        (warn $ "Not using an implicit direction to overwrite an implicitly set map direction of room " <> "") --show r1)
      -- and don't bother if there's any connection at all
      unless (isJust $ r1 ^? connectionLens opp) $ modifyRoom r1 (makeConnection Implicit dir r2)
    pass) (handleMissingObject "failed to make direction" ())

makeDirections True ["West"]

-- ~\~ end
