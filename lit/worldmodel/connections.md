# Making Connections

```haskell file=src/Yaifl/Core/Objects/Room.hs
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Core.Objects.Room
  --( isWestOf
  --, getMapConnection
where
 -- ) where

import Cleff.State ( State )
import qualified Data.Map as Map
import qualified Data.Text.Lazy.Builder as TLB
import Display ( displayText )
import Solitude

import Yaifl.Core.Common ( Entity, HasID(getID), Metadata, WMDirections, whenConstructingM )
import Yaifl.Core.Directions ( HasOpposite(opposite), WithDirections )
import Yaifl.Core.Logger ( warn, Log )
import Yaifl.Core.Objects.Object ( objData, Room )
import Yaifl.Core.Objects.ObjectData ( MapConnections(MapConnections), Connection(..), ConnectionExplicitness(..), roomMapConnections )
import Yaifl.Core.Objects.Query

hasSpecificConnectionTo ::
  ObjectQuery wm es
  => State (Metadata wm) :> es
  => ObjectLike wm o
  => WithDirections wm
  => Maybe ConnectionExplicitness 
  -> o
  -> WMDirections wm
  -> Eff es (Maybe Entity)
hasSpecificConnectionTo mbExpl o dir = do
  r <- getRoomMaybe o
  let v = getConnectionInDirection dir =<< r
  case v of
    Just (Connection ex' e) 
      | (maybe True (ex' ==) mbExpl) -> return $ Just e
    _ -> return Nothing

getMapConnection ::
  NoMissingObjects wm es
  => WithDirections wm
  => ObjectLike wm o
  => WMDirections wm
  -> o
  -> Eff es (Maybe Entity)
getMapConnection dir o = ((_connectionRoom <$>) . getConnectionInDirection dir) <$> getRoom o

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
  (ObjectLike wm o1, ObjectLike wm o2)
  => ObjectQuery wm es
  => State (Metadata wm) :> es
  => Log :> es
  => WithDirections wm
  => WMDirections wm
  -> o1
  -> o2
  -> Eff es Entity
addDirectionFrom = isDirectionFromInternal True

addDirectionFromOneWay ::
  (ObjectLike wm o1, ObjectLike wm o2)
  => '[State (Metadata wm), Log] :>> es
  => ObjectQuery wm es
  => WithDirections wm
  => WMDirections wm
  -> o1
  -> o2
  -> Eff es Entity
addDirectionFromOneWay = isDirectionFromInternal False 

isDirectionFromInternal ::
  (ObjectLike wm o1, ObjectLike wm o2)
  => '[State (Metadata wm), Log] :>> es
  => ObjectQuery wm es
  => WithDirections wm
  => Bool
  -> WMDirections wm

  -> o1
  -> o2
  -> Eff es Entity
isDirectionFromInternal mkRev dir o1 o2 = withoutMissingObjects (do
    let opp = opposite dir
    -- ensure we have two rooms
    r2 <- getRoom o2
    r1 <- getRoom o1
    -- we log a warning if we're in construction and we are overriding an explicit connection
    -- apparently inform just doesn't let you do this, so...
    -- r1 is explicitly dir of r2; it is r2 we need to check
    -- r2 is implicitly (opposite dir) of r1.
    -- e.g. if r1 `isWestOf` r2, then r2 has an explicit west connection and r1 has an implicit east connection.
    whenConstructingM (isJust <$> hasSpecificConnectionTo (Just Explicit) r2 dir)
      -- TODO: this should be a nonblocking failure
      (warn $ TLB.fromText $ "Overriding an explicitly set map direction of room " <> displayText r1) 
    modifyRoom r2 (makeConnection Explicit dir r1)
    --only make the reverse if we want to
    when mkRev $ do
      -- something weird is happening if we're overriding an implicit direction with another implicit direction
      -- but I think in general we don't bother setting an implicit one
      whenConstructingM (isJust <$> hasSpecificConnectionTo (Just Implicit) r2 opp)
        (warn $ TLB.fromText $ "Not using an implicit direction to overwrite an implicitly set map direction of room " <> displayText r1) 
      -- and don't bother if there's any connection at all
      unless (isJust $ r1 ^? connectionLens opp) $ modifyRoom r1 (makeConnection Implicit dir r2)
    return (getID o1)) (handleMissingObject "failed to make direction" (return $ getID o1))

-- makeDirections True ["West"]

```
