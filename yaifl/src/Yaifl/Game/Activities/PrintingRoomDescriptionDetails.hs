module Yaifl.Game.Activities.PrintingRoomDescriptionDetails
  ( printingRoomDescriptionDetailsImpl
  , WithPrintingRoomDescriptionDetails

  ) where

import Yaifl.Prelude
import Yaifl.Model.Activity
import Yaifl.Model.Kinds
import Yaifl.Model.Rules.Rulebook
import Yaifl.Model.HasProperty
import Yaifl.Model.Kinds.Enclosing
import Yaifl.Text.Responses

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