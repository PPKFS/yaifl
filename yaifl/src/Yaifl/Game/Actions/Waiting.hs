{-# LANGUAGE RecordWildCards #-}
module Yaifl.Game.Actions.Waiting where

import Yaifl.Model.Action
import Yaifl.Prelude
import Yaifl.Core.Actions.Args
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.Say
import Yaifl.Core.Metadata
import Yaifl.Core.Actions.GoesWith

data WaitingResponses wm

type WaitingAction wm = Action wm () 'TakesNoParameter ()
type WaitingRule wm = ActionRule wm (WaitingAction wm) ()

waitingAction :: WithPrintingNameOfSomething wm => WaitingAction wm
waitingAction = (makeAction "waiting")
  { name = "waiting"
  , understandAs = ["wait", "z"]
  , parseArguments = actionOnNothing
  , reportRules = makeActionRulebook "report waiting rulebook" [ standardReportWaiting ]
  }

standardReportWaiting :: WithPrintingNameOfSomething wm => WaitingRule wm
standardReportWaiting = makeRule "standard waiting rule" [] $ \Args{..} -> do
  ifM (isPlayer source)
    (unless (silently actionOptions) $
      [saying|Time #{pass}.|]
    )
    [saying|{The source} #{wait}.|]
  rulePass