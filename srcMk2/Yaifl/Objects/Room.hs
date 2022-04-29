{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Objects.Room
  ( isWestOf
  , getMapConnection
    
  ) where

import Yaifl.Directions
import Yaifl.Common
import Solitude
import Yaifl.Objects.ObjectData
import Yaifl.Objects.Missing
import Yaifl.WorldInfo
import Yaifl.Objects.Query
import Display
import Yaifl.Logger
import qualified Data.Text.Lazy.Builder as TLB
import Yaifl.Objects.Object
import qualified Data.Map as Map
import Yaifl.Properties.TH


hasSpecificConnectionTo ::
  MonadWorld wm m
  => ObjectLike wm o
  => WithDirections wm
  => Explicitness 
  -> o
  -> WMDirections wm
  -> m (Maybe Entity)
hasSpecificConnectionTo expl o dir = do
  r <- getRoomMaybe o
  case r ^? _Just % objData % roomMapConnections % to unMapConnections % at dir % _Just of
    Just (Connection ex' e) 
      | ex' == expl -> return $ Just e
    _ -> return Nothing

-- | why does this use `NoMissingObjects` but return `Maybe`? We want to only call this when it
-- makes sense (i.e. when we have a room for sure) but the connection isn't a guarantee
getMapConnection ::
  forall wm m o.
  NoMissingObjects m
  => MonadWorld wm m
  => ObjectLike wm o
  => WithDirections wm
  => o
  -> WMDirections wm
  -> m (Maybe Entity)
getMapConnection o dir = do
  r <- getRoom o
  return $ r ^? objData % roomMapConnections % to unMapConnections % at dir % _Just % connectionRoom

makeConnection :: 
  forall wm.
  WithDirections wm
  => Explicitness
  -> WMDirections wm 
  -> Room wm 
  -> (Room wm -> Room wm)
makeConnection expl dir r = 
  objData % roomMapConnections % coercedTo @(Map.Map (WMDirections wm) Connection ) % at dir ?~ Connection expl (getID r)

isDirectionFrom ::
  MonadWorld wm m
  => WithDirections wm
  => WMDirections wm
  -> m Entity
  -> Entity
  -> m Entity
isDirectionFrom = isDirectionFromInternal True

isDirectionFromOneWay ::
  MonadWorld wm m
  => WithDirections wm
  => WMDirections wm
  -> m Entity
  -> Entity
  -> m Entity
isDirectionFromOneWay = isDirectionFromInternal False 

isDirectionFromInternal ::
  MonadWorld wm m
  => WithDirections wm
  => Bool
  -> WMDirections wm
  -> m Entity
  -> Entity
  -> m Entity
isDirectionFromInternal mkRev dir constrObj e = withoutMissingObjects (do
  -- run the construction
  mkObj <- lift constrObj
  let opp = opposite dir
  -- ensure we have two rooms
  r1 <- getRoom e
  r0 <- getRoom mkObj
  -- we log a warning if we're in construction and we are overriding an explicit connection
  -- apparently inform just doesn't let you do this, so...
  -- r0 is explicitly dir of r1; it is r1 we need to check
  -- r1 is implicitly (opposite dir) of r0.
  whenConstructingM (isJust <$> hasSpecificConnectionTo Explicit r1 dir)
    -- TODO: this should be a nonblocking failure
    (warn $ TLB.fromText $ "Overriding an explicitly set map direction of room " <> displayText r1) 
  modifyRoom mkObj (makeConnection Explicit dir r0)
  --only make the reverse if we want to
  when mkRev $ do
    -- something weird is happening if we're overriding an implicit direction with another implicit direction
    -- but I think in general we don't bother setting an implicit one
    whenConstructingM (isJust <$> hasSpecificConnectionTo Implicit r1 opp)
      (warn $ TLB.fromText $ "Not using an implicit direction to overwrite an implicitly set map direction of room " <> displayText r0) 
    -- and don't bother if there's any connection at all
    unless (isJust $ r1 ^? objData % roomMapConnections % to unMapConnections % at opp) $
      modifyRoom mkObj (makeConnection Implicit dir r0)
  return mkObj) (handleMissingObject "failed to make direction" constrObj)

makeDirections True ["West"]