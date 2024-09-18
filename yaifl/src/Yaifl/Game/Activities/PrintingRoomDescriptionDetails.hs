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
  (const $ notImplementedResponse "localeparagraph")
  (blankRulebook "before printing a locale paragraph")
  ((blankRulebook "carry out printing a locale paragraph")
    { rules =
      [
      ]
    })
  (blankRulebook "after printing a locale paragraph")