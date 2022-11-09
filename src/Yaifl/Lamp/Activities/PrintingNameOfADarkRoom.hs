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

import Solitude ( ($), Monad((>>)) )

import Yaifl.Core.Actions.Activity ( Activity, makeActivity )
import Yaifl.Core.Rulebooks.Rule (rulePass, makeRule')
import Yaifl.Core.Say ( say )

printingNameOfADarkRoomImpl :: Activity o () ()
printingNameOfADarkRoomImpl = makeActivity "Printing name of a dark room"
    $ makeRule' "" (say "Darkness" >> rulePass)