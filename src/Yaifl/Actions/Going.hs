{-# LANGUAGE RecordWildCards #-}
module Yaifl.Actions.Going
  (goingAction) where

import Solitude

import Yaifl.Actions.Action
import Yaifl.Model.Entity ( Entity )
import Yaifl.Metadata ( isPlayer )
import Yaifl.Model.Object( Thing, Room, isType, AnyObject )
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.ThingData
import Yaifl.Rules.Args
import Yaifl.Rules.Rule
import Yaifl.Rules.RuleEffects
import Yaifl.Model.Direction
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Door
import Yaifl.Model.Objects.RoomConnections
import Yaifl.Model.Objects.Effects
import Yaifl.Text.SayQQ
import Yaifl.Text.Say
import Breadcrumbs
import Yaifl.Model.Objects.Move

data GoingActionVariables wm = GoingActionVariables
  { --The going action has a room called the room gone from (matched as "from").
    roomGoneFrom :: Room wm
    --The going action has an object called the room gone to (matched as "to").
  , roomGoneTo :: Room wm
    --The going action has an object called the door gone through (matched as "through").
  --, doorGoneThrough :: Maybe (Door wm)
    --The going action has an object called the vehicle gone by (matched as "by").
  , vehicleGoneBy :: Maybe (Thing wm)
    --The going action has an object called the thing gone with (matched as "with").
  , thingGoneWith :: Maybe (Thing wm)
  } deriving stock ( Generic )

goingAction ::
  (WMStdDirections wm, WMHasProperty wm DoorSpecifics)
  => WithPrintingNameOfSomething wm
  => Action wm (GoingActionVariables wm)
goingAction = Action
  "going"
  ["go", "going"]
  (TakesOneOf TakesDirectionParameter TakesObjectParameter)
  (map (,TakesObjectParameter) ["with", "through", "by", "to"])
  (ParseArguments goingActionSet)
  (makeActionRulebook "before going rulebook" [])
  (makeActionRulebook "check going rulebook" checkGoingRules)
  carryOutGoingRules
  (makeActionRulebook "report going rulebook" [ describeRoomGoneInto ])

describeRoomGoneInto :: Rule wm (Args wm (GoingActionVariables wm)) Bool
describeRoomGoneInto = notImplementedRule "describe room gone into rule"

carryOutGoingRules :: ActionRulebook wm (GoingActionVariables wm)
carryOutGoingRules = makeActionRulebook "carry out going rulebook"
  [ movePlayerAndVehicle
  , moveFloatingObjects
  , checkLightInNewLocation
  ]

checkLightInNewLocation :: Rule wm (Args wm (GoingActionVariables wm)) Bool
checkLightInNewLocation = notImplementedRule "check light in new location rule"

moveFloatingObjects :: Rule wm (Args wm (GoingActionVariables wm)) Bool
moveFloatingObjects = notImplementedRule "move floating objects rule"

movePlayerAndVehicle :: Rule wm (Args wm (GoingActionVariables wm)) Bool
movePlayerAndVehicle = makeRule "move player and vehicle rule" [] $ \v -> do
  moveSuccessful <- case vehicleGoneBy v of
    Nothing -> move (source v) (roomGoneTo v)
    Just x -> error ""
  error ""


goingActionSet ::
  forall wm es.
  (ParseArgumentEffects wm es, WMStdDirections wm, WMHasProperty wm DoorSpecifics)
  => WithPrintingNameOfSomething wm
  => UnverifiedArgs wm
  -> Eff es (ArgumentParseResult (GoingActionVariables wm))
goingActionSet (UnverifiedArgs Args{..}) = do
  --now the thing gone with is the item-pushed-between-rooms;
  thingGoneWith <- getMatchingThing "with"
  -- now the room gone from is the location of the actor;
  roomGoneFrom <- getLocation source
  --if the actor is in an enterable vehicle (called the carriage), now the vehicle gone by is the carriage;
  vehicleGoneBy <- actorInEnterableVehicle source
  {-
    if we are going in a direction, then we want to find the door or room in D from the current room
    if we are going to a door (my add: or through a door), then choose the door
    after this, if there is a door in the way then we are clearly going through the door and our target is through the door
    and of course now the room we're going to is on the other side of the door.
  -}
  -- find all the possible targets we could mean
  target <- case fst variables of
    -- if the noun is a direction:
    -- let direction D be the noun;
    -- let the target be the room-or-door direction D from the room gone from;
    DirectionParameter dir -> do
      addAnnotation $ "going in direction " <> show dir
      addAnnotation $ "possible exits are " <> show (roomGoneFrom ^. #objectData % #mapConnections)
      pure $ getMapConnection @wm dir roomGoneFrom
    -- if the noun is a door, let the target be the noun;
    -- now the door gone through is the target;
    -- now the target is the other side of the target from the room gone from;
    ObjectParameter door -> setDoorGoneThrough door
    NoParameter -> do
      mbThrough <- getMatchingThing "through"
      mbDoor <- join <$> traverse getDoorSpecifics mbThrough
      pure $ backSide <$> mbDoor
  mbRoomGoneTo <- join <$> traverse getRoomMaybe target
  addAnnotation $ "target was " <> show target
  case mbRoomGoneTo of
    Nothing -> cantGoThatWay source =<< getMatchingThing "through"
    Just roomGoneTo -> pure $ Right $ GoingActionVariables
      { thingGoneWith
      , roomGoneFrom
      , roomGoneTo = roomGoneTo
      , vehicleGoneBy
      }

cantGoThatWay ::
  RuleEffects wm es
  => WithPrintingNameOfSomething wm
  => Thing wm
  -> Maybe (Thing wm)
  -> Eff es (ArgumentParseResult a)
cantGoThatWay source mbDoorThrough = do
  whenM (isPlayer source) $
    case mbDoorThrough of
      -- say "[We] [can't go] that way." (A);
      Nothing -> [saying|#{We} #{can't go} that way.|]
      Just door -> [saying|#{We} #{can't}, since {the door} #{lead} nowhere.|]
  pure $ Left "Can't go that way"

getMatchingThing :: RuleEffects wm es => Text -> Eff es (Maybe (Thing wm))
getMatchingThing matchElement = do
  e <- getMatching matchElement
  case e of
    Nothing -> pure Nothing
    Just e' -> getThingMaybe e'

setDoorGoneThrough :: AnyObject wm -> Eff es (Maybe Entity)
setDoorGoneThrough = error ""

getDoorMaybe :: Thing wm -> AnyObject wm
getDoorMaybe = error ""

actorInEnterableVehicle :: Thing wm4 -> Eff es (Maybe (Thing wm))
actorInEnterableVehicle _ = pure Nothing

getMatching :: Text -> Eff es (Maybe Entity)
getMatching = const $ return Nothing

checkGoingRules ::
  [Rule wm (Args wm (GoingActionVariables wm)) Bool]
checkGoingRules = [
  standUpBeforeGoing
  , cantTravelInNotAVehicle
  , cantGoThroughUndescribedDoors
  , cantGoThroughClosedDoors
  ]

cantGoThroughClosedDoors :: Rule wm (Args wm (GoingActionVariables wm)) Bool
cantGoThroughClosedDoors = notImplementedRule "stand up before going"

cantGoThroughUndescribedDoors :: Rule wm (Args wm (GoingActionVariables wm)) Bool
cantGoThroughUndescribedDoors = notImplementedRule "stand up before going"

cantTravelInNotAVehicle :: Rule wm (Args wm (GoingActionVariables wm)) Bool
cantTravelInNotAVehicle = makeRule "can't travel in what's not a vehicle" [] $ \v -> do
  nonVehicle <- getObject $ v ^. #source % #objectData % #containedBy
  let _vehcGoneBy = v ^. #variables % #vehicleGoneBy
      _roomGoneFrom = v ^. #variables % #roomGoneFrom
  -- if nonvehicle is the room gone from, continue the action; if nonvehicle is the vehicle gone by, continue the action;
  error "" --ruleCondition' (pure $ not ((nonVehicle `objectEquals` roomGoneFrom) || maybe True (`objectEquals` nonVehicle) vehcGoneBy) )
  whenM (isPlayer $ v ^. #source) $ do
    _outAction <- ifM (nonVehicle `isType` "supporter") (pure ("off" :: String)) (pure "out of")
   -- let dir = "[We] [would have] to get off [the nonvehicle] first."
    pass
  rulePass

standUpBeforeGoing ::
  Rule wm (Args wm (GoingActionVariables wm)) Bool
standUpBeforeGoing = makeRule "stand up before going" [] $ \_v -> do error ""
  {-chaises <- error ""--ruleCondition (nonEmpty <$> getSupportersOf (v ^. #source))
  res <- forM chaises (\chaise -> do
      whenM (isPlayer $ v ^. #source) $ do
        printText "(first getting off "
        printName chaise
        printLn ")"
      parseAction (ActionOptions True (Just $ v ^. #source)) "exit")
  if any isLeft res then return $ Just False else rulePass-}

getContainingHierarchy ::
  NoMissingObjects wm es
  => Thing wm
  -> Eff es [Thing wm]
getContainingHierarchy o = do
  cont <- getThingMaybe (o ^. #objectData % #containedBy)
  case cont of
    --no objects, just a room
    Nothing -> return []
    Just ob -> (ob:) <$> getContainingHierarchy ob

getSupportersOf ::
  NoMissingObjects wm es
  => Thing wm
  -> Eff es [Thing wm]
getSupportersOf o = getContainingHierarchy o >>= filterM (`isType` "supporter")