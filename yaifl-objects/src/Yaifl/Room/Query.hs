module Yaifl.Room.Query
  ( getAllObjectsInRoom
  , addDoorToConnection
  , isWestOf
  , isSouthOf
  , isEastOf
  , isNorthOf
  , isSouthWestOf
  , isWestOfOneWay
  , isSouthOfOneWay
  , isEastOfOneWay
  , isNorthOfOneWay
  , isInsideFrom
  , isOutsideFrom
  , isAbove
  , isBelow
  , isNowhere
  , getMapConnection
  , getConnection
  , getAllConnections
  , getConnectionViaDoor
  , isNowMapped
  , addDirectionFrom
  ) where

import Yaifl.Prelude

import Yaifl.Object.Kind
import Yaifl.Effects.ObjectQuery
import Yaifl.Enclosing.Kind
import Yaifl.Room.Kind
import Yaifl.Enclosing.Query
import Yaifl.Thing.Kind
import Yaifl.ObjectLike
import Yaifl.Tag
import Yaifl.Property.Has
import Yaifl.Entity
import Yaifl.Direction.Kind
import Yaifl.TH
import Yaifl.Effects.RuleEffects
import Yaifl.Metadata
import Yaifl.Room.Connection
import Yaifl.Object.Query
import Breadcrumbs

getAllObjectsInRoom ::
  RoomLike wm o
  => WMWithProperty wm Enclosing
  => WithoutMissingObjects wm es
  => IncludeScenery
  -> IncludeDoors
  -> RecurseAllObjects
  -> o
  -> Eff es [Thing wm]
getAllObjectsInRoom incScenery incDoors recurse r = do
  r' <- getRoom r
  getAllObjectsInEnclosing incScenery incDoors recurse (coerceTag $ tagRoomEntity r')

addDirectionFrom ::
  HasCallStack
  => WMStdDirections wm
  => WithoutMissingObjects wm es
  => WMDirection wm
  -> RoomEntity
  -> RoomEntity
  -> Eff es ()
addDirectionFrom = isDirectionFromInternal True

addDirectionFromOneWay ::
  WMStdDirections wm
  => WithoutMissingObjects wm es
  => WMDirection wm
  -> RoomEntity
  -> RoomEntity
  -> Eff es ()
addDirectionFromOneWay = isDirectionFromInternal False

isNowMapped ::
  WMStdDirections wm
  => WithoutMissingObjects wm es
  => RoomEntity
  -> WMDirection wm
  -> RoomEntity
  -> Eff es ()
isNowMapped roomTo dir = isDirectionFromInternal False dir roomTo

inDirection ::
  WMStdDirections wm
  => WithoutMissingObjects wm es
  => "thisRoom" :! RoomEntity
  -> "leads" :! WMDirection wm
  -> "here" :! RoomEntity
  -> "isOneWay" :? Bool
  -> Eff es ()
inDirection (arg #thisRoom -> tr) (arg #leads -> l) (arg #here -> t) (argDef #isOneWay False -> o) = isDirectionFromInternal (not o) l t tr

-- | the ordering here is that roomIsOf' `isSouthOf` (for example) baseRoom meanss
-- the connection to be made explicitly is from baseRoom (south) -> roomIsOf
-- then the implicit reverse connection is roomIsOf (opposite south) -> baseRoom
isDirectionFromInternal ::
  HasCallStack
  => WMStdDirections wm
  => WithoutMissingObjects wm es
  => Bool
  -> WMDirection wm
  -> RoomEntity
  -> RoomEntity
  -> Eff es ()
isDirectionFromInternal mkRev dir roomIsOfE baseRoomE = do
    let opp = opposite dir
    baseRoom <- getRoom baseRoomE
    roomIsOf <- getRoom roomIsOfE
    -- we log a warning if we're in construction and we are overriding an explicit connection
    -- apparently inform just doesn't let you do this, so...
    -- roomIsOf is explicitly dir of baseRoom; it is baseRoom we need to check
    -- roomIsOf is implicitly (opposite dir) of baseRoom.
    -- e.g. if baseRoom `isWestOf` roomIsOf, then roomIsOf has an explicit west connection and baseRoom has an implicit east connection.
    whenConstructing (isJust $ connectionInDirection (Just Explicit) baseRoom dir)
      -- TODO: this should be a nonblocking failure
      (addAnnotation $ "Overriding an explicitly set map direction of room " <> "")--show baseRoom)
    modifyRoom baseRoom (makeConnection Explicit dir roomIsOf)
    --only make the reverse if we want to
    when mkRev $ do
      -- something weird is happening if we're overriding an implicit direction with another implicit direction
      -- but I think in general we don't bother setting an implicit one
      whenConstructing (isJust $ connectionInDirection (Just Implicit) roomIsOf opp)
        (addAnnotation $ "Not using an implicit direction to overwrite an implicitly set map direction of room " <> "") --show baseRoom)
      -- and don't bother if there's any connection at all
      if isNothing $ roomIsOf ^. connectionLens opp
        then do
          modifyRoom roomIsOf (makeConnection Implicit opp baseRoom)
          addAnnotation $ "made implicit connection from " <> display (view #name roomIsOf) <> " going " <> show opp <> " to " <> display (view #name baseRoom)
        else
          addAnnotation $ "did not make implicit connection from " <> display (view #name roomIsOf) <> " going " <> show opp <> " to " <> display (view #name baseRoom)
            <> " because it's already made."
    addAnnotation $ "made connection from " <> display (view #name baseRoom) <> " going " <> show dir <> " to " <> display (view #name roomIsOf)

makeDirections True ["West", "South", "North", "East", "In", "Out", "Up", "Down", "SouthWest", "SouthEast", "NorthWest", "NorthEast"]

isInsideFrom ::
  WMStdDirections wm
  => WithoutMissingObjects wm es
  => RoomEntity
  -> RoomEntity
  -> Eff es ()
isInsideFrom = isInOf

isOutsideFrom ::
  WMStdDirections wm
  => WithoutMissingObjects wm es
  => RoomEntity
  -> RoomEntity
  -> Eff es ()
isOutsideFrom = isOutOf

isAbove ::
  HasCallStack
  => WMStdDirections wm
  => WithoutMissingObjects wm es
  => RoomEntity
  -> RoomEntity
  -> Eff es ()
isAbove = isUpOf

isBelow ::
  WMStdDirections wm
  => WithoutMissingObjects wm es
  => RoomEntity
  -> RoomEntity
  -> Eff es ()
isBelow = isDownOf

addDoorToConnection ::
  WMStdDirections wm
  => RuleEffects wm es
  => DoorEntity
  -> (RoomEntity, WMDirection wm)
  -> (RoomEntity, WMDirection wm)
  -> Eff es ()
addDoorToConnection d (front, frontDir) (back, backDir) = do
  modifyAndVerifyConnection front frontDir back (#doorThrough ?~ d)
  modifyAndVerifyConnection back backDir front (#doorThrough ?~ d)

-- | Check that a connection exists from a given room and, if it does, then modify it.
-- this only modifies the forward direction!
modifyAndVerifyConnection ::
  forall wm es.
  WMStdDirections wm
  => RuleEffects wm es
  => RoomEntity
  -> WMDirection wm
  -> RoomEntity
  -> (Connection wm -> Connection wm)
  -> Eff es ()
modifyAndVerifyConnection fromRoomE' fromDir destE f = do
  fromRoom <- getRoom fromRoomE'
  case connectionInDirection Nothing fromRoom fromDir of
    -- all good
    Just dest
      | dest == destE ->
        modifyRoom @wm fromRoom (connectionLens fromDir % _Just %~ f)
    Just anotherDest -> do
      let r = display $ fromRoom ^. #name
      r2 <- getRoom anotherDest
      let r2' = display (r2 ^. #name)
      noteError (const ()) ("When modifying the connection from " <> r <> " we expected the other side to be "
        <> show destE <> ". but it was " <> r2' <> " in the direction " <> show fromDir)
    Nothing -> do
      inDirection ! #thisRoom fromRoomE' ! #leads fromDir ! #here destE ! #isOneWay True
      modifyRoom @wm fromRoomE' (connectionLens fromDir % _Just %~ f)

isNowhere ::
  forall wm es.
  WMStdDirections wm
  => WithoutMissingObjects wm es
  => RoomEntity
  -> WMDirection wm
  -> Eff es ()
isNowhere r d = do
  fromRoom <- getRoom r
  modifyRoom @wm fromRoom (connectionLens d .~ Nothing)
