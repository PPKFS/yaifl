module Yaifl.Activities.PrintingThePlayersObituary
  ( printingThePlayersObituaryImpl
  , WithPrintingThePlayersObituary

  ) where

import Yaifl.Prelude
import Yaifl.Activity
import Yaifl.Rulebook

type WithPrintingThePlayersObituary wm =
  ( WithActivity "printingThePlayersObituary" wm () () ()
  )

type PrintingThePlayersObituaryRule wm = ActivityRule wm () () ()

printingThePlayersObituaryImpl :: Activity wm () () ()
printingThePlayersObituaryImpl = makeActivity "printing the player's obituary" [makeRule "printing the player's obituary" [] (const rulePass) ]