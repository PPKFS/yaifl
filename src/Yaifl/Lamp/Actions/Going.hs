{-|
Module      : Yaifl.Actions.Going
Description : The going action.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}


module Yaifl.Lamp.Actions.Going
  (goingAction) where

import Solitude
import Effectful

import Yaifl.Core.Actions.Action ( makeActionRulebook, Action(Action), ActionRulebook )
import Yaifl.Core.Direction ( WMStdDirections )
import Yaifl.Core.Entity ( Entity, HasID(..) )
import Yaifl.Core.Metadata (noteError, isPlayer)
import Yaifl.Core.Object ( Thing, Room, objData, isType, objectEquals )
import Yaifl.Core.Objects.Query
import Yaifl.Core.Objects.Room ( getMapConnection )
import Yaifl.Core.Objects.ThingData (thingContainedBy)
import Yaifl.Core.Properties.Has ( WMHasProperty )
import Yaifl.Core.Rulebooks.Args ( ArgSubject(..) )
import Yaifl.Core.Rulebooks.Rule
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Say (say, sayLn)
import Yaifl.Lamp.Activities.PrintingNameOfSomething ( printNameDefiniteUncapitalised )
import Yaifl.Lamp.Properties.Door ( Door(..), getDoor )

data GoingActionVariables wm = GoingActionVariables
  { --The going action has a room called the room gone from (matched as "from").
    _gavRoomFrom :: Room wm
    --The going action has an object called the room gone to (matched as "to").
  , _gavRoomTo :: Room wm
    --The going action has an object called the door gone through (matched as "through").
  , _gavDoorGoneThrough :: Maybe (Thing wm)
    --The going action has an object called the vehicle gone by (matched as "by").
  , _gavVehicleGoneBy :: Maybe (Thing wm)
    --The going action has an object called the thing gone with (matched as "with").
  , _gavThingGoneWith :: Maybe (Thing wm)
  }

makeLenses ''GoingActionVariables

goingAction :: (WMStdDirections wm, WMHasProperty wm Door) => Action wm
goingAction = Action
  "going"
  ["go", "going"]
  ["with", "through", "by", "to"]
  (ParseArguments goingActionSet)
  (makeActionRulebook "before going rulebook" [])
  (makeActionRulebook "check going rulebook" checkGoingRules)
  carryOutGoingRules
  (makeActionRulebook "report going rulebook" [])

carryOutGoingRules :: ActionRulebook wm v0
carryOutGoingRules = makeActionRulebook "carry out going rulebook" []

goingActionSet ::
  forall wm es.
  (ParseArgumentEffects wm es, WMStdDirections wm, WMHasProperty wm Door)
  => UnverifiedArgs wm
  -> Eff es (ArgumentParseResult (GoingActionVariables wm))
goingActionSet a@(UnverifiedArgs Args{..}) = withoutMissingObjects (do
  --now the thing gone with is the item-pushed-between-rooms;
  goneWith <- getMatching "with" >>= maybe (return Nothing) getThingMaybe
  -- now the room gone from is the location of the actor;
  roomFrom <- getRoom =<< getLocation _argsSource
  --if the actor is in an enterable vehicle (called the carriage), now the vehicle gone by is the carriage;
  vehicleGoneBy <- actorInEnterableVehicle _argsSource
  {-
    if we are going in a direction, then we want to find the door or room in D from the current room
    if we are going to a door (my add: or through a door), then choose the door
    after this, if there is a door in the way then we are clearly going through the door and our target is through the door
    and of course now the room we're going to is on the other side of the door.
  -}
  let (n :: [ArgSubject wm]) = getNouns a
  -- find all the possible targets we could mean
  targets <- catMaybes <$> mapM (\case
      -- if the noun is a direction
      -- let direction D be the noun and let the target be the room-or-door direction D from the room gone from
      DirectionSubject d -> pure $ getMapConnection @wm d roomFrom
      -- if the noun is a door, let the target be the noun;
      RegularSubject r -> setDoorGoneThrough r
      -- this is also the door case, but it matches "go through red door", whereas the above case will match "go red door"
      MatchedSubject "through" n' -> setDoorGoneThrough n'
      -- todo: this should be a big ol' error case for the rest
      ConceptSubject _ -> pure Nothing
      MatchedSubject _ _ -> pure Nothing
      ) n
  -- ensure we have 1 target, which is either a room (passthrough) or a door (in which case we want to go through it)
  target <- case targets of
    [] -> noteError Left "I have no idea where you wanted to go."
    --we have too many targets; this probably arises if we try to go east via the west door
    (_:_:_) -> noteError Left "I have no idea where you wanted to go; you seemed to suggest multiple directions."
    -- this should either be a room or a door
    [x] -> asThingOrRoomM x
      (\thing -> do
        --if it's not a door, then we've messed up.
        mbDoor <- getDoor thing
        case mbDoor of
          Nothing -> noteError Left [int|t|You said you wanted to go to the TODO which makes no sense.|]
          Just d ->
            --get the other side of the door. we don't check (TODO) whether the other side is indeed a room.
            if
              getID roomFrom == _backSide d
            then Right . (Just thing,) <$> getLocation thing
            else Right . (Just thing,) <$> getRoom (_backSide d)
          )
      -- if it's a room directly, problem solved.
      (pure . Right . (Nothing,))
  let gav b a' = GoingActionVariables
        { _gavRoomFrom = roomFrom
        , _gavRoomTo = a'
        , _gavDoorGoneThrough = b
        , _gavVehicleGoneBy = vehicleGoneBy
        , _gavThingGoneWith = goneWith
        }
  case target of
    Left txt -> return $ Left txt
    Right r -> return $ Right $ uncurry gav r ) (handleMissingObject "Failed to set going variables" (Left ""))

setDoorGoneThrough :: Entity -> Eff (NoMissingObject : es) (Maybe Entity)
setDoorGoneThrough = error ""

actorInEnterableVehicle :: Thing wm4 -> Eff es (Maybe (Thing wm))
actorInEnterableVehicle _ = pure Nothing

getNouns :: UnverifiedArgs wm  -> [ArgSubject wm]
getNouns = _argsVariables . unArgs

getMatching :: Text -> Eff es (Maybe Entity)
getMatching = const $ return Nothing

checkGoingRules :: [Rule wm (Args wm (GoingActionVariables wm)) Bool]
checkGoingRules = [
  standUpBeforeGoing
  , cantTravelInNotAVehicle
  , cantGoThroughUndescribedDoors
  , cantGoThroughClosedDoors
  ]

cantGoThroughClosedDoors :: Rule wm (Args wm (GoingActionVariables wm)) Bool
cantGoThroughClosedDoors = makeRule "stand up before going" $ \v -> do
  return Nothing

cantGoThroughUndescribedDoors :: Rule wm (Args wm (GoingActionVariables wm)) Bool
cantGoThroughUndescribedDoors = makeRule "stand up before going" $ \v -> do
  return Nothing

cantTravelInNotAVehicle :: Rule wm (Args wm (GoingActionVariables wm)) Bool
cantTravelInNotAVehicle = makeRule "can't travel in what's not a vehicle" $ \v -> do
  nonVehicle <- getObject $ v ^. argsSource % objData % thingContainedBy
  let vehcGoneBy = v ^. argsVariables % gavVehicleGoneBy
      roomGoneFrom = v ^. argsVariables % gavRoomFrom
  -- if nonvehicle is the room gone from, continue the action; if nonvehicle is the vehicle gone by, continue the action;
  ruleCondition' (pure $ not ((nonVehicle `objectEquals` roomGoneFrom) || maybe True (`objectEquals` nonVehicle) vehcGoneBy) )
  whenM (isPlayer $ v ^. argsSource) $ do
    outAction <- ifM (nonVehicle `isType` "supporter") (pure "off") (pure "out of")
    let dir = "[We] [would have] to get off [the nonvehicle] first."
    pass
  rulePass

standUpBeforeGoing :: Rule wm (Args wm (GoingActionVariables wm)) Bool
standUpBeforeGoing = makeRule "stand up before going" $ \v -> do
  chaises <- ruleCondition (nonEmpty <$> getSupportersOf (v ^. argsSource))
  res <- forM chaises (\chaise -> do
      whenM (isPlayer $ v ^. argsSource) $ do
        say "(first getting off "
        printNameDefiniteUncapitalised chaise
        sayLn ")"
      parseAction (ActionOptions True (Just $ v ^. argsSource)) "exit")
  if any isLeft res then return $ Just False else rulePass

getContainingHierarchy ::
  NoMissingObjects wm es
  => Thing wm
  -> Eff es [Thing wm]
getContainingHierarchy o = do
  cont <- getThingMaybe (o ^. objData % thingContainedBy)
  case cont of
    --no objects, just a room
    Nothing -> return []
    Just ob -> (ob:) <$> getContainingHierarchy ob

getSupportersOf ::
  NoMissingObjects wm es
  => Thing wm
  -> Eff es [Thing wm]
getSupportersOf o = getContainingHierarchy o >>= filterM (`isType` "supporter")