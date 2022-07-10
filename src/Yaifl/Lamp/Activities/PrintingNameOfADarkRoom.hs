{-|
Module      : Yaifl.Actions.Action
Description : An action is a verb that is carried out by the player (or an NPC).
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}


module Yaifl.Lamp.Activities.PrintingNameOfADarkRoom
  ( printingNameOfADarkRoomImpl
  ) where

import Yaifl.Core.Actions.Activity ( Activity, makeActivity )

import Yaifl.Core.Say
import Yaifl.Core.Rulebooks.Rule (rulePass, makeRule')

printingNameOfADarkRoomImpl :: Activity o () ()
printingNameOfADarkRoomImpl = makeActivity "Printing name of a dark room"
    $ makeRule' "" (say "Darkness" >> rulePass)