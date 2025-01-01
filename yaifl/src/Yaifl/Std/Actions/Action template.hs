{-# LANGUAGE RecordWildCards #-}
module Yaifl.Std.Actions.SwitchingOn where

import Yaifl.Core.Action
import Yaifl.Prelude
import Yaifl.Core.Actions.Args
import Yaifl.Core.Rules.RuleEffects
import Yaifl.Text.SayableValue
import Yaifl.Text.Responses
import Yaifl.Core.Kinds.Object
import Yaifl.Std.Kinds.Direction
import Yaifl.Core.WorldModel
import Yaifl.Text.Say (SayableValue(..), sayText)
import Yaifl.Text.SayQQ

data SwitchingOnResponses wm

type SwitchingOnAction wm = Action wm () 'TakesThingParameter (Thing wm)
switchingOnAction :: SwitchingOnAction wm
switchingOnAction = (makeAction "switching on")
  { name = "switching on"
  , understandAs = error ""
  , matches = error ""
  , parseArguments = error ""
  , beforeRules = makeActionRulebook "before switching on rulebook" []
  , insteadRules = makeActionRulebook "instead of switching on rulebook" []
  , checkRules = makeActionRulebook "check switching on rulebook" []
  , carryOutRules = makeActionRulebook "carry out switching on rulebook" []
  , reportRules = makeActionRulebook "report switching on rulebook" []
  }