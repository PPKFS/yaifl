module Yaifl.Game.Activities.PrintingInventoryDetails
  ( printingInventoryDetailsImpl
  , WithPrintingInventoryDetails

  ) where

import Yaifl.Prelude
import Yaifl.Model.Activity
import Yaifl.Model.Kinds
import Yaifl.Model.Rules.Rulebook
import Yaifl.Core.HasProperty
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Text.Responses

type WithPrintingInventoryDetails wm =
  ( WithActivity "printingInventoryDetails" wm () (Thing wm) ()
  , WMWithProperty wm Enclosing
  )

type PrintingInventoryDetailsRule wm = ActivityRule wm () (Thing wm) ()

printingInventoryDetailsImpl :: Activity wm () (Thing wm) ()
printingInventoryDetailsImpl = Activity "printing room description details about something" Nothing Nothing
  (const $ notImplementedResponse "printing room description details about something")
  (blankRulebook "before printing room description details about something")
  ((blankRulebook "carry out printing room description details about something")
    { rules =
      [
      ]
    })
  (blankRulebook "after printing room description details about something")
  (const)