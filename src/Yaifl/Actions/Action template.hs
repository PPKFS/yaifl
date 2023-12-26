{-# LANGUAGE RecordWildCards #-}
module Yaifl.Actions.Opening where


import Yaifl.Actions.Action
import Solitude
import Yaifl.Rules.Args
import Yaifl.Rules.Rule
import Yaifl.Text.Responses
import Yaifl.Model.Object
import Yaifl.Model.Direction
import Yaifl.Model.WorldModel
import Yaifl.Text.Say (SayableValue(..), sayText)
import Yaifl.Text.SayQQ

data OpeningResponses wm

type OpeningAction wm = Action wm ('TakesOneOf 'TakesDirectionParameter 'TakesObjectParameter) (Thing wm)
openingAction :: OpeningAction wm
openingAction = Action
  { name = error ""
  , understandAs = error ""
  , matches = error ""
  , parseArguments = error ""
  , beforeRules = makeActionRulebook "before opening rulebook" []
  , insteadRules = makeActionRulebook "instead of opening rulebook" []
  , checkRules = makeActionRulebook "check opening rulebook" []
  , carryOutRules = makeActionRulebook "carry out opening rulebook" []
  , reportRules = makeActionRulebook "report opening rulebook" []

  }