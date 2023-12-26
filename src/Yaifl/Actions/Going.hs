{-# LANGUAGE RecordWildCards #-}
module Yaifl.Actions.Going
  ( goingAction
  , GoingActionVariables(..)
  , GoingResponses
  , toTheRoom
  , throughTheDoor
  , throughTheClosedDoor
  ) where

import Solitude

import Yaifl.Actions.Action
import Yaifl.Model.Objects.Entity
import Yaifl.Metadata ( isPlayer )
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query
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
import Yaifl.Model.Properties.Enclosing
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Display
import Yaifl.Model.WorldModel (WMDirection)
import Yaifl.Model.Objects.Tag
import Yaifl.Model.Objects.ObjectLike
import Yaifl.Model.Properties.Openable

data GoingActionVariables wm = GoingActionVariables
  { --The going action has a room called the room gone from (matched as "from").
    roomGoneFrom :: Room wm
    --The going action has an object called the room gone to (matched as "to").
  , roomGoneTo :: Room wm
    --The going action has an object called the door gone through (matched as "through").
  , doorGoneThrough :: Maybe DoorEntity
    --The going action has an object called the vehicle gone by (matched as "by").
  , vehicleGoneBy :: Maybe (Thing wm)
    --The going action has an object called the thing gone with (matched as "with").
  , thingGoneWith :: Maybe (Thing wm)
  } deriving stock ( Generic )

data GoingResponses = GR ()

type GoingAction wm = Action wm GoingResponses ('Optionally ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter)) (GoingActionVariables wm)
type GoingRule wm = ActionRule wm (GoingAction wm) (GoingActionVariables wm)
goingAction ::
  (WMStdDirections wm, WMWithProperty wm DoorSpecifics, WMWithProperty wm Enclosing)
  => WithPrintingNameOfSomething wm
  => GoingAction wm
goingAction = Action
  "going"
  ["go", "going"]
  (map (,TakesObjectParameter) ["with", "through", "by", "to"])
  (\x -> error "")
  (ParseArguments goingActionSet)
  (makeActionRulebook "before going rulebook" [])
  (makeActionRulebook "instead of going rulebook" [])
  (makeActionRulebook "check going rulebook" checkGoingRules)
  carryOutGoingRules
  (makeActionRulebook "report going rulebook" [ describeRoomGoneInto ])

checkGoingRules ::
  [GoingRule wm]
checkGoingRules = [
  standUpBeforeGoing
  , cantTravelInNotAVehicle
  , cantGoThroughUndescribedDoors
  , cantGoThroughClosedDoors
  -- we've already done the checking the direction bit, but maybe we'll need to do it again?
  -- determine map connction
  -- can't go that way
  ]

carryOutGoingRules :: WMWithProperty wm Enclosing => ActionRulebook wm (GoingAction wm) (GoingActionVariables wm)
carryOutGoingRules = makeActionRulebook "carry out going rulebook"
  [ movePlayerAndVehicle
  , moveFloatingObjects
  , checkLightInNewLocation
  ]

-- todo: other actors moving, or if the player is moved via another actor
describeRoomGoneInto :: GoingRule wm
describeRoomGoneInto = makeRule "describe room gone into rule" [] $ \a -> ifM
  (isPlayer (source a))
  (unless (silently . actionOptions $ a) (void $ do
    sayLn @Text ""
    parseAction ((actionOptions a) { silently = True }) [ConstantParameter "going"] "look") >> rulePass)
  (error "other actors cant report going yet")

checkLightInNewLocation :: GoingRule wm
checkLightInNewLocation = notImplementedRule "check light in new location rule"

moveFloatingObjects :: GoingRule wm
moveFloatingObjects = notImplementedRule "move floating objects rule"

movePlayerAndVehicle :: WMWithProperty wm Enclosing => GoingRule wm
movePlayerAndVehicle = makeRule "move player and vehicle rule" [] $ \a@Args{variables=v} -> do
  moveSuccessful <- case vehicleGoneBy v of
    Nothing -> move (source a) (roomGoneTo v)
    Just _x -> error "failed to move with a vehicle"
  pure $ if moveSuccessful then Nothing else Just False

goingActionSet ::
  forall wm es.
  (ParseArgumentEffects wm es, WMStdDirections wm, WMWithProperty wm DoorSpecifics)
  => WithPrintingNameOfSomething wm
  => UnverifiedArgs wm ('Optionally ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter))
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
  mbTargetAndConn <- case fst variables of
    -- if the noun is a direction:
    -- let direction D be the noun;
    -- let the target be the room-or-door direction D from the room gone from;
    Just (Left dir) -> do
      addAnnotation $ "going in direction " <> show dir
      addAnnotation $ "possible exits are " <> show (roomGoneFrom ^. #objectData % #mapConnections)
      pure $ getConnection @wm dir roomGoneFrom
    -- if the noun is a door, let the target be the noun;
    -- now the door gone through is the target;
    -- now the target is the other side of the target from the room gone from;
    Just (Right door) -> pure $ (\ds -> getConnectionViaDoor (tag ds (getID door)) roomGoneFrom) =<< getDoorSpecificsMaybe door
    Nothing -> do
      mbThrough <- getMatchingThing "through"
      pure $ do
            door <- mbThrough
            ds <- getDoorSpecificsMaybe door
            getConnectionViaDoor (tag ds (getID door)) roomGoneFrom
  case mbTargetAndConn of
    Nothing -> flip (cantGoThatWay source) roomGoneFrom =<< getMatchingThing "through"
    Just (target, conn) -> do
      mbRoomGoneTo <- getRoomMaybe target
      case mbRoomGoneTo of
        Nothing -> flip (cantGoThatWay source) roomGoneFrom =<< getMatchingThing "through"
        Just roomGoneTo -> do
          addAnnotation $ "target was " <> show target
          pure $ Right $ GoingActionVariables
            { thingGoneWith
            , roomGoneFrom
            , doorGoneThrough = conn ^. #doorThrough
            , roomGoneTo = roomGoneTo
            , vehicleGoneBy
            }

cantGoThatWay ::
  RuleEffects wm es
  => Display (WMDirection wm)
  => WithPrintingNameOfSomething wm
  => Thing wm
  -> Maybe (Thing wm)
  -> Room wm
  -> Eff es (ArgumentParseResult a)
cantGoThatWay source mbDoorThrough fromRoom = do
  whenM (isPlayer source) $ do
    let possExits = Map.keys $ getAllConnections fromRoom
    case mbDoorThrough of
      -- say "[We] [can't go] that way." (A);
      Nothing -> do
        rn <- sayText (fromRoom ^. #name)
        [saying|#{We} #{can't go} that way.|]
        sayLn $ " Perhaps we could try one of " <> T.intercalate ", " (map display possExits) <> " out of " <> rn <> " ?"
      Just door -> [sayingLn|#{We} #{can't}, since {the door} #{lead} nowhere.|]
  pure $ Left "Can't go that way"

getMatchingThing :: RuleEffects wm es => Text -> Eff es (Maybe (Thing wm))
getMatchingThing matchElement = do
  e <- getMatching matchElement
  case e of
    Nothing -> pure Nothing
    Just e' -> getThingMaybe e'

setDoorGoneThrough :: AnyObject wm -> Eff es (Maybe RoomEntity)
setDoorGoneThrough _ = pure Nothing

getDoorMaybe :: Thing wm -> AnyObject wm
getDoorMaybe = error ""

actorInEnterableVehicle :: Thing wm4 -> Eff es (Maybe (Thing wm))
actorInEnterableVehicle _ = pure Nothing

getMatching :: Text -> Eff es (Maybe Entity)
getMatching = const $ return Nothing

cantGoThroughClosedDoors :: GoingRule wm
cantGoThroughClosedDoors = notImplementedRule "stand up before going"

cantGoThroughUndescribedDoors :: GoingRule wm
cantGoThroughUndescribedDoors = notImplementedRule "stand up before going"

cantTravelInNotAVehicle :: GoingRule wm
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

standUpBeforeGoing :: GoingRule wm
standUpBeforeGoing = makeRule "stand up before going" [] $ \_v -> do error ""
  {-chaises <- error ""--ruleCondition (nonEmpty <$> getSupportersOf (v ^. #source))
  res <- forM chaises (\chaise -> do
      whenM (isPlayer $ v ^. #source) $ do
        printText "(first getting off "
        printName chaise
        printLn ")"
      parseAction (ActionOptions True (Just $ v ^. #source)) "exit")
  if any isLeft res then return $ Just False else rulePass-}

getContainingThingHierarchy ::
  NoMissingObjects wm es
  => Thing wm
  -> Eff es [Thing wm]
getContainingThingHierarchy o = do
  cont <- getThingMaybe (o ^. #objectData % #containedBy)
  case cont of
    --no objects, just a room
    Nothing -> return []
    Just ob -> (ob:) <$> getContainingThingHierarchy ob

getSupportersOf ::
  NoMissingObjects wm es
  => Thing wm
  -> Eff es [Thing wm]
getSupportersOf o = getContainingThingHierarchy o >>= filterM (`isType` "supporter")

toTheRoom ::
  ObjectLike wm r
  => r
  -> Precondition wm (Args wm (GoingActionVariables wm))
toTheRoom r = Precondition (getObject r >>= \o -> pure $ "to the room " <> display (o ^. #name)) $ \v -> do
  pure $ getID (roomGoneTo $ variables v) == getID r

throughTheDoor ::
  forall d wm.
  TaggedAs d DoorTag
  => d
  -> Precondition wm (Args wm (GoingActionVariables wm))
throughTheDoor d = Precondition (pure "through a specific door") $ \v -> do
  pure $ (getID <$> doorGoneThrough (variables v)) == Just (getID $ toTag @d @DoorTag d)

throughTheClosedDoor ::
  forall d wm.
  TaggedAs d DoorTag
  => WMWithProperty wm Openability
  => ThingLike wm d
  => d
  -> Precondition wm (Args wm (GoingActionVariables wm))
throughTheClosedDoor d = Precondition (pure "through a specific closed door") $ \v -> do
  o <- getThing d
  pure $
    isClosed o &&
    (getID <$> doorGoneThrough (variables v)) == Just (getID $ toTag @d @DoorTag d)