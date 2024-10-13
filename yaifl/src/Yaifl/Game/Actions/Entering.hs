{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.Entering where

import Yaifl.Model.Action
import Yaifl.Prelude
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Responses
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Kinds.Direction
import Yaifl.Model.WorldModel
import Yaifl.Text.Say (SayableValue(..), sayText)
import Yaifl.Text.SayQQ
import Yaifl.Model.Kinds
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Game.Move
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Tag
import Yaifl.Model.Entity (EnclosingTag)
import Yaifl.Model.Query (getEnclosingObject)

data EnteringResponses wm

type EnteringAction wm = Action wm () 'TakesThingParameter (TaggedContainer wm)

-- TODO: "supplying a missing noun rulebook"

enteringAction :: (WMWithProperty wm Enclosing, WMWithProperty wm Container) => EnteringAction wm
enteringAction = (makeAction "entering")
  { name = "entering"
  , understandAs = ["enter", "go in", "go into", "enter into", "get into", "get in", "get on"]
  , parseArguments = ParseArguments $ \(UnverifiedArgs Args{..}) -> do
      let mbCont = getContainerMaybe (fst variables)
      case mbCont of
        Nothing -> return $ Left "that's not enterable"
        Just x -> return $ Right (tagObject x (fst variables))
  , beforeRules = makeActionRulebook "before entering rulebook" []
  , insteadRules = makeActionRulebook "instead of entering rulebook" []
  , checkRules = makeActionRulebook "check entering rulebook"
    [ convertEnterDoor
    , convertEnterDirection
    , cantEnterWhenEntered
    , cantEnterUnenterable
    , cantEnterClosedContainers
    , cantExceedCapacity
    ]
  , carryOutRules = makeActionRulebook "carry out entering rulebook" [ standardEntering ]
  , reportRules = makeActionRulebook "report entering rulebook" []
  }

type EnteringRule wm = ActionRule wm (EnteringAction wm) (TaggedContainer wm)

convertEnterDoor :: EnteringRule wm
convertEnterDoor = notImplementedRule "convert enter door"

convertEnterDirection :: EnteringRule wm
convertEnterDirection = notImplementedRule "convert enter direction"

cantEnterWhenEntered :: EnteringRule wm
cantEnterWhenEntered = notImplementedRule "cant enter what's already entered"

cantEnterUnenterable :: EnteringRule wm
cantEnterUnenterable = notImplementedRule "can't enter what's not enterable rule"

cantEnterClosedContainers :: EnteringRule wm
cantEnterClosedContainers = notImplementedRule "can't enter closed containers rule"

cantExceedCapacity :: EnteringRule wm
cantExceedCapacity = notImplementedRule "can't enter if this exceeds carrying capacity"

standardEntering :: WMWithProperty wm Enclosing => EnteringRule wm
standardEntering = makeRule "standard entering" [] $ \a@Args{variables=v} -> do
  moveSuccessful <- move (source a) v
  pure $ if moveSuccessful then Nothing else Just False
