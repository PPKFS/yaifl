module Yaifl.Activities.PrintingInventoryDetails
  ( printingInventoryDetailsImpl
  , WithPrintingInventoryDetails

  ) where

import Yaifl.Prelude
import Yaifl.Activity
import Yaifl.Enclosing.Kind
import Yaifl.Text.Responses
import Yaifl.Thing.Kind
import Yaifl.Rulebook
import Yaifl.Property.Has

type WithPrintingInventoryDetails wm =
  ( WithActivity "printingInventoryDetails" wm () (Thing wm) ()
  , WMWithProperty wm Enclosing
  )

type PrintingInventoryDetailsRule wm = ActivityRule wm () (Thing wm) ()

printingInventoryDetailsImpl :: Activity wm () (Thing wm) ()
printingInventoryDetailsImpl = Activity
  { name = "printing room description details about something"
  , defaultOutcome = Nothing
  , currentVariables = Nothing
  , responses = (const $ notImplementedResponse "printing room description details about something")
  , beforeRules = (blankRulebook "before printing room description details about something")
  , carryOutRules = ((blankRulebook "carry out printing room description details about something") {rules = []})
  , afterRules = (blankRulebook "after printing room description details about something"), combineResults = const
  }