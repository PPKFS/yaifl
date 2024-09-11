{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.SwitchingOn where

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

data SwitchingOnResponses wm

type SwitchingOnAction wm = Action wm ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter) (Thing wm)
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