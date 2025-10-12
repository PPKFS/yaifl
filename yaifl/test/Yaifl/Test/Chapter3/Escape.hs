module Yaifl.Test.Chapter3.Escape
  ( ex21

  ) where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Core.Kinds.Object
import Yaifl.Std.Create.Object
import Yaifl.Std.EffectHandlers
import Yaifl.Std.ObjectSpecifics
import Yaifl.Core.Metadata
import Yaifl.Test.Common
import Yaifl.Core.Tag
import Yaifl.Std.Kinds.Direction
import Yaifl.Std.Create
import Yaifl.Text.SayableValue
import Yaifl.Std.Actions.Imports
import Yaifl.Std.Actions.Going


ex21 :: (Text, [Text], Game PlainWorldModel ())
ex21 = ("Escape", escapeTestMeWith, escapeWorld)

escapeWorld :: Game PlainWorldModel ()
escapeWorld = do
  setTitle "Escape"
  yb <- addRoom "Your Bedroom" ! done
  gs <- addRoom "Grassy Slope" ! #modify makeNameImproper ! done
  w <- addDoor "bedroom window"
    ! #front (yb, East)
    ! #back (gs, West)
    ! done
  insteadOf #searching [theObject w] $ \_ -> do
    bs <- getOtherSideOfDoor w
    [saying|Through the window, you make out {the bs}.|]
  insteadOf #climbing [theObject w] $ tryAction "enter" [TheThing $ coerceTag w]

  -- the original requires you to define "climb through [something]" as an alias, whereas
  -- my parser will just assume you want to climb something called the "through window" and considers
  -- one word enough of a match.

  -- I don't know if this needs fixing but if I leave this here for when I inevitably rewrite the parser it'll help.
  insteadOf #going [throughTheClosedDoor w] $ const [saying|The window is shut: you'd break the glass.|]
  pass

{-
Your Bedroom
You can see a bedroom window here.
>[1] look through window
Through the window, you make out the Grassy Slope.
>[2] climb through window
The window is shut: you'd break the glass.
>[3] open window
You open the bedroom window.
>[4] climb through window
Grassy Slope
You can see a bedroom window here.
>[5] look through window
Through the window, you make out Your Bedroom.
>[6] close window
You close the bedroom window.
>[7] e
The window is shut: you'd break the glass.
>[8] open window
You open the bedroom window.
>[9] e
Your Bedroom
You can see a bedroom window here.
-}
escapeTestMeWith :: [Text]
escapeTestMeWith = fromI7TestMe [wrappedText|look through window / climb window / open window / climb through window / look through window / close window / e / open window / e|]