{-|
Module      : Yaifl.Actions.Action
Description : An action is a verb that is carried out by the player (or an NPC).
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}


module Yaifl.Core.Activities.PrintingADarkRoom
  ( printingNameOfADarkRoomImpl
  ) where

import Yaifl.Core.Activities.Activity

import Yaifl.Core.Rulebooks.Rulebook
import Yaifl.Core.Say

printingNameOfADarkRoomImpl :: Activity o () ()
printingNameOfADarkRoomImpl = makeActivity "Printing name of a dark room"
    $ makeRule' "" (say "Darkness" >> rulePass)