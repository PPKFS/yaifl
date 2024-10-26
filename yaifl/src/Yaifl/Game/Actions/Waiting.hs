module Yaifl.Game.Actions.Waiting where

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

data WaitingResponses wm

type WaitingAction wm = Action wm () 'TakesNoParameter ()

waitingAction :: WaitingAction wm
waitingAction = (makeAction "waiting")
  { name = "waiting"
  , understandAs = error ""
  , matches = error ""
  , parseArguments = error ""
  , beforeRules = makeActionRulebook "before waiting rulebook" []
  , insteadRules = makeActionRulebook "instead of waiting rulebook" []
  , checkRules = makeActionRulebook "check waiting rulebook" []
  , carryOutRules = makeActionRulebook "carry out waiting rulebook" []
  , reportRules = makeActionRulebook "report waiting rulebook" []
  }