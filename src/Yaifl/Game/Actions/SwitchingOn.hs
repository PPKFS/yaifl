{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.SwitchingOn where

import Yaifl.Model.Action
import Solitude
import Yaifl.Model.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Say
import Yaifl.Text.SayQQ
import Yaifl.Model.Kinds.Thing
import Yaifl.Model.Kinds.Device
import Yaifl.Model.HasProperty

type SwitchingOnAction wm = Action wm () 'TakesThingParameter (Thing wm)

switchingOnAction ::
  WithPrintingNameOfSomething wm
  => WMWithProperty wm Device
  => SwitchingOnAction wm
switchingOnAction = (makeAction "switching on")
  { understandAs = ["switch on", "switching on", "turn on", "turning on"]
  , parseArguments = actionOnOneThing
  , checkRules = makeActionRulebook "check switching on rulebook" [ onlyIfSwitchable, onlyIfNotAlreadyOn ]
  , carryOutRules = makeActionRulebook "carry out switching on rulebook" [ standardSwitchingOn ]
  , reportRules = makeActionRulebook "report switching on rulebook"  [ standardReportSwitchingOn ]
  }

onlyIfSwitchable :: ActionRule wm (SwitchingOnAction wm) (Thing wm)
onlyIfSwitchable = notImplementedRule "can't switch on unless switchable rule"

onlyIfNotAlreadyOn :: ActionRule wm (SwitchingOnAction wm) (Thing wm)
onlyIfNotAlreadyOn = notImplementedRule "can't switch on what's already on rule"

standardSwitchingOn :: WMWithProperty wm Device => ActionRule wm (SwitchingOnAction wm) (Thing wm)
standardSwitchingOn = makeRule "standard switching on rule" [] $ \thing -> do
  -- now the noun is switched on.
  switchItOn (variables thing)
  rulePass

standardReportSwitchingOn :: WithPrintingNameOfSomething wm => ActionRule wm (SwitchingOnAction wm) (Thing wm)
standardReportSwitchingOn = makeRule "standard report switching on rule" [] $ \args -> do
  let actor = source args
      noun = variables args
  -- if the action is not silent:
  unless (silently . actionOptions $ args)
    [saying|{The actor} #{switch} {the noun} on.|]
    --say "[The actor] [switch] [the noun] on." (A).
  rulePass