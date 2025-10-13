module Yaifl.Std.Activities.PrintingRoomDescriptionDetails
  ( printingRoomDescriptionDetailsImpl
  , WithPrintingRoomDescriptionDetails

  ) where

import Yaifl.Prelude
import Yaifl.Core.Activity
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Text.Responses
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Rules.Rulebook
import Yaifl.Core.HasProperty

type WithPrintingRoomDescriptionDetails wm =
  ( WithActivity "printingRoomDescriptionDetails" wm () (Thing wm) ()
  , WMWithProperty wm Enclosing
  )

type PrintingRoomDescriptionDetailsRule wm = ActivityRule wm () (Thing wm) ()

printingRoomDescriptionDetailsImpl :: Activity wm () (Thing wm) ()
printingRoomDescriptionDetailsImpl = Activity
  { name = "printing room description details about something"
  , defaultOutcome = Nothing
  , currentVariables = Nothing
  , responses = const $ notImplementedResponse "printing room description details about something"
  , beforeRules = blankRulebook "before printing room description details about something"
  , carryOutRules = blankRulebook "carry out printing room description details about something"
  , afterRules = blankRulebook "after printing room description details about something"
  , combineResults = const
  }
