module Yaifl.Activities.PrintingDescriptionOfADarkRoom
(
    printingDescriptionOfADarkRoomImpl
) where

import Yaifl.Activities.Common
import Yaifl.Rulebooks
import Yaifl.Messages
TODO: extract
printingDescriptionOfADarkRoomImpl :: Activity o () ()
printingDescriptionOfADarkRoomImpl = makeActivity "Printing description of a dark room"
    $ makeRule' "" (say "It is pitch black, and you can't see a thing." >> rulePass)