{-# LANGUAGE RecordWildCards #-}
module Yaifl.Std.Actions.Waiting where

import Yaifl.Std.Actions.Imports
import Yaifl.Prelude
import Yaifl.Core.Metadata

data WaitingResponses wm

type WaitingAction wm = Action wm () 'TakesNoParameter ()
type WaitingRule wm = ActionRule wm (WaitingAction wm) ()

waitingAction :: WithPrintingNameOfSomething wm => WaitingAction wm
waitingAction = (makeAction "waiting")
  { name = "waiting"
  , understandAs = ["wait", "z"]
  , parseArguments = actionOnNothing
  , carryOutRules = makeActionRulebook "carry out waiting rulebook" []
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