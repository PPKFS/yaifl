module Yaifl.Activities.PrintingADarkRoom
(
    printingNameOfADarkRoomImpl
) where

import Yaifl.Activities.Common
import Yaifl.Common
import Yaifl.Prelude
import Yaifl.Rulebooks
import Yaifl.Messages
printingNameOfADarkRoomImpl :: Activity o () ()
printingNameOfADarkRoomImpl = makeActivity "Printing name of a dark room"
    $ makeRule' "" (\w -> (Just (), say "Darkness" w))