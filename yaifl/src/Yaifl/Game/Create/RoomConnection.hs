

module Yaifl.Game.Create.RoomConnection
  ( isWestOf
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
  , addDoorToConnection
  , getConnectionViaDoor
  , isNowMapped
  , isNowOn
  , addDirectionFrom
  ) where

import qualified Data.Map as Map

import Breadcrumbs

import Data.Text.Display

import Yaifl.Model.Metadata ( whenConstructing, noteError )
import Yaifl.Model.Kinds.Direction
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Effects
import Yaifl.Model.Entity
import Yaifl.Model.ObjectLike
import Yaifl.Model.Query
import Yaifl.Model.Kinds.Room
import Yaifl.Model.TH ( makeDirections, WMWithProperty )
import Yaifl.Model.WorldModel ( WMDirection )

import qualified Data.Map as M
import Yaifl.Text.Say
import Yaifl.Model.Rules (RuleEffects)
import Yaifl.Prelude hiding (Down)
import Yaifl.Model.Kinds.Supporter (SupporterEntity)
import Yaifl.Game.Move (move)
import Yaifl.Model.Tag
import Yaifl.Model.Kinds.Enclosing

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

connectionLens ::
  forall wm.
  WMStdDirections wm
  => WMDirection wm
  -> Lens' (Room wm) (Maybe (Connection wm))
connectionLens dir = (#objectData :: Lens' (Room wm) (RoomData wm)) % #mapConnections % coercedTo @(Map.Map (WMDirection wm) (Connection wm)) % at dir

makeConnection ::
  WMStdDirections wm
  => ConnectionExplicitness
  -> WMDirection wm
  -> Room wm
  -> (Room wm -> Room wm)
makeConnection expl dir r = connectionLens dir ?~ Connection expl (tagRoom r) Nothing dir

addDirectionFrom ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => WMDirection wm
  -> RoomEntity
  -> RoomEntity
  -> Eff es ()
addDirectionFrom = isDirectionFromInternal True

addDirectionFromOneWay ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => WMDirection wm
  -> RoomEntity
  -> RoomEntity
  -> Eff es ()
addDirectionFromOneWay = isDirectionFromInternal False

isNowMapped ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => RoomEntity
  -> WMDirection wm
  -> RoomEntity
  -> Eff es ()
isNowMapped roomTo dir = isDirectionFromInternal False dir roomTo

inDirection ::
  WMStdDirections wm
  => NoMissingObjects wm es
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
  WMStdDirections wm
  => NoMissingObjects wm es
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
  => NoMissingObjects wm es
  => RoomEntity
  -> RoomEntity
  -> Eff es ()
isInsideFrom = isInOf

isOutsideFrom ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => RoomEntity
  -> RoomEntity
  -> Eff es ()
isOutsideFrom = isOutOf

isAbove ::
  WMStdDirections wm
  => NoMissingObjects wm es
  => RoomEntity
  -> RoomEntity
  -> Eff es ()
isAbove = isUpOf

isBelow ::
  WMStdDirections wm
  => NoMissingObjects wm es
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
      r <- sayText (fromRoom ^. #name)
      r2 <- getRoom anotherDest
      r2' <- sayText (r2 ^. #name)
      noteError (const ()) ("When modifying the connection from " <> r <> " we expected the other side to be "
        <> show destE <> ". but it was " <> r2' <> " in the direction " <> show fromDir)
    Nothing -> do
      inDirection ! #thisRoom fromRoomE' ! #leads fromDir ! #here destE ! #isOneWay True
      modifyRoom @wm fromRoomE' (connectionLens fromDir % _Just %~ f)

isNowhere ::
  forall wm es.
  WMStdDirections wm
  => NoMissingObjects wm es
  => RoomEntity
  -> WMDirection wm
  -> Eff es ()
isNowhere r d = do
  fromRoom <- getRoom r
  modifyRoom @wm fromRoom (connectionLens d .~ Nothing)

isNowOn ::
  RuleEffects wm es
  => WMWithProperty wm Enclosing
  => ThingLike wm t
  => t
  -> SupporterEntity
  -> Eff es ()
isNowOn t e = do
  t' <- getThing t
  e' <- getEnclosingObject (coerceTag e)
  let e'' = TaggedObject (coerceTag @_ @EnclosingTag e, fst e')
  void $ move t' e''