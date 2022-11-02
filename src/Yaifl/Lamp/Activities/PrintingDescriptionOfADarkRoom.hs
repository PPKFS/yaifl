{-|
Module      : Yaifl.Actions.Action
Description : An action is a verb that is carried out by the player (or an NPC).
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}


module Yaifl.Lamp.Activities.PrintingDescriptionOfADarkRoom
( printingDescriptionOfADarkRoomImpl
) where

import Yaifl.Core.Actions.Activity ( Activity, makeActivity )
import Yaifl.Core.Say
import Yaifl.Core.Rulebooks.Rule
import Solitude


printingDescriptionOfADarkRoomImpl :: Activity o () ()
printingDescriptionOfADarkRoomImpl = makeActivity "Printing description of a dark room"
    $ makeRule' "" (say "It is pitch black, and you can't see a thing." >> rulePass)