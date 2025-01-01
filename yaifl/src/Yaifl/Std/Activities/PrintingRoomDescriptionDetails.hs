module Yaifl.Std.Activities.PrintingRoomDescriptionDetails
  ( printingRoomDescriptionDetailsImpl
  , WithPrintingRoomDescriptionDetails

  ) where

import Yaifl.Prelude
import Yaifl.Core.Activity
import Yaifl.Core.Kinds.Enclosing
import Yaifl.Text.Responses
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.WorldModel
import Yaifl.Core.Rules.Rulebook

type WithPrintingRoomDescriptionDetails wm =
  ( WithActivity "printingRoomDescriptionDetails" wm () (Thing wm) ()
  , WMWithProperty wm Enclosing
  )

type PrintingRoomDescriptionDetailsRule wm = ActivityRule wm () (Thing wm) ()

printingRoomDescriptionDetailsImpl :: Activity wm () (Thing wm) ()
printingRoomDescriptionDetailsImpl = Activity "printing room description details about something" Nothing Nothing
  (const $ notImplementedResponse "printing room description details about something")
  (blankRulebook "before printing room description details about something")
  ((blankRulebook "carry out printing room description details about something")
    { rules =
      [
      ]
    })
  (blankRulebook "after printing room description details about something")
  (const)