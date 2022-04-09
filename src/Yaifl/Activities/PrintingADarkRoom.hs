{-|
Module      : Yaifl.Actions.Action
Description : An action is a verb that is carried out by the player (or an NPC).
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}


module Yaifl.Activities.PrintingADarkRoom
  ( printingNameOfADarkRoomImpl
  ) where

import Yaifl.Activities.Activity
import Solitude
import Yaifl.Rulebooks.Rulebook
import Yaifl.Say

printingNameOfADarkRoomImpl :: Activity o () ()
printingNameOfADarkRoomImpl = makeActivity "Printing name of a dark room"
    $ makeRule' "" (say "Darkness" >> rulePass)