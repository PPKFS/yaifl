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

module Yaifl.Lamp.Actions.Going
  (goingActionImpl) where


import Yaifl.Core.Actions.Action
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Common
import Yaifl.Core.Objects.Object
import Yaifl.Core.Objects.Move
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Args
import Yaifl.Core.Objects.Room
import Yaifl.Lamp.Actions.Looking
import Yaifl.Core.Directions

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

goingActionName :: Text 
goingActionName = "looking"

goingActionImpl :: (WMStdDirections wm) => Action wm
goingActionImpl = Action
  goingActionName
  ["with", "through", "by", "to"]
  ["go", "going"]
  (ParseArguments goingActionSet)
  (makeActionRulebook "before going rulebook" [])
  (makeActionRulebook "check going rulebook" [])
  carryOutGoingRules
  (makeActionRulebook "report going rulebook" [])

carryOutGoingRules :: ActionRulebook wm v0
carryOutGoingRules = makeActionRulebook "carry out going rulebook" []

goingActionSet :: 
  (ParseArgumentEffects wm es, WMStdDirections wm)
  => UnverifiedArgs wm
  -> Eff es (ArgumentParseResult (GoingActionVariables wm))
goingActionSet (UnverifiedArgs Args{..}) = withoutMissingObjects (do
  --now the thing gone with is the item-pushed-between-rooms;
  goneWithT <- getMatching "with"
  goneWith <- maybe (pure Nothing) getThingMaybe goneWithT
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
  (n :: [ArgSubject wm]) <- getNouns
  targets <- catMaybes <$> mapM (\case
      -- if the noun is a direction
      -- let direction D be the noun and let the target be the room-or-door direction D from the room gone from
      DirectionSubject d -> pure $ getMapConnection @wm d roomFrom
      -- if the noun is a door, let the target be the noun;
      RegularSubject r -> setDoorGoneThrough r
      -- this is also the door case, but it matches "go through red door", whereas the above case will match "go red door"
      MatchedSubject "through" n -> setDoorGoneThrough n
      -- todo: this should be a big ol' error case for the rest
      ConceptSubject _ -> pure Nothing
      MatchedSubject _ _ -> pure Nothing
      ) n
  case targets of
    [] -> error ""--we have no idea where to go
    [x] -> error ""
    (x:xs) -> error ""
  return $ Left "err") (handleMissingObject "" (Left  "err"))

setDoorGoneThrough :: Entity -> Eff (NoMissingObject : es) (Maybe Entity)
setDoorGoneThrough = error ""

actorInEnterableVehicle :: Thing wm4 -> Eff (NoMissingObject : es) a2
actorInEnterableVehicle = error ""

getNouns :: Eff es [ArgSubject wm]
getNouns = error "not implemented"

getMatching :: t1 -> Eff es (Maybe Entity)
getMatching = error "not implemented"
