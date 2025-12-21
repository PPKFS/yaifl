module Yaifl.Std.Activities.PrintingInventoryDetails
  ( printingInventoryDetailsImpl
  , WithPrintingInventoryDetails

  ) where

import Yaifl.Prelude
import Yaifl.Core.Activity
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