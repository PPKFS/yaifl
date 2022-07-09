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

module Yaifl.Core.Actions.Going
  (goingActionImpl) where

import Solitude
import Yaifl.Core.Actions.Action
import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Common
import Yaifl.Core.Objects.Object
import Yaifl.Core.Objects.Move (getLocation)
import Yaifl.Core.Objects.Missing
import Yaifl.Core.Objects.Query
import Yaifl.Core.Rulebooks.Args
import Yaifl.Core.Objects.Room

data GoingActionVariables wm = GoingActionVariables
  { _gavRoomFrom :: Room wm
  , _gavRoomTo :: Room wm
  , _gavDoorGoneThrough :: Maybe (Thing wm)
  , _gavVehicleGoneBy :: Maybe (Thing wm)
  , _gavThingGoneWith :: Maybe (Thing wm)
  }


goingActionName :: Text 
goingActionName = "looking"

goingActionImpl :: Action wm
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
  forall wm m.
  Monad m 
  => UnverifiedArgs wm 
  -> m (Maybe (GoingActionVariables wm))
goingActionSet (UnverifiedArgs Args{..}) = withoutMissingObjects (runMaybeT $ do
  --now the thing gone with is the item-pushed-between-rooms;
  goneWith <- getMatching "with" >>= getThingMaybe
  -- now the room gone from is the location of the actor;
  roomFrom <- getRoom <$> getLocation _argsSource
  --if the actor is in an enterable vehicle (called the carriage), now the vehicle gone by is the carriage;
  vehicleGoneBy <- actorInEnterableVehicle _argsSource
  {-     
    if we are going in a direction, then we want to find the door or room in D from the current room
    if we are going to a door (my add: or through a door), then choose the door
    after this, if there is a door in the way then we are clearly going through the door and our target is through the door
    and of course now the room we're going to is on the other side of the door.
  -}
  (n :: Maybe (ArgSubject wm)) <- getNoun
  target <- case n of
    -- if the noun is a direction, let direction D be the noun and let the target be the room-or-door 
    -- direction D from the room gone from
    Just (DirectionSubject d) -> getMapConnection @wm roomFrom d
    Just (RegularSubject r) -> pure Nothing
    Nothing -> pure Nothing --lacking any direction
    
  vl <- getVisibilityLevels reifyLoc
  lightLevels <- recalculateLightOfParent t
  return $ GoingActionVariables reifyLoc lightLevels (take lightLevels vl) "looking") (handleMissingObject "" $ return Nothing)

getNoun :: m (Maybe (ArgSubject wm))
getNoun = error "not implemented"

getMatching :: t1 -> m (Maybe a)
getMatching = error "not implemented"
