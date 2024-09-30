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

data EnteringResponses wm

type EnteringAction wm = Action wm () 'TakesThingParameter (Maybe (Thing wm))

-- TODO: "supplying a missing noun rulebook"

enteringAction :: EnteringAction wm
enteringAction = (makeAction "entering")
  { name = "entering"
  , understandAs = ["enter", "go in", "go into", "enter into"]
  , parseArguments = error ""
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
  , carryOutRules = makeActionRulebook "carry out entering rulebook" []
  , reportRules = makeActionRulebook "report entering rulebook" []
  }

type EnteringRule wm = ActionRule wm (EnteringAction wm) (Maybe (Thing wm))

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