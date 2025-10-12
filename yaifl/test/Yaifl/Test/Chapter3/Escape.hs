module Yaifl.Test.Chapter3.Escape
  ( ex21

  ) where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

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


ex21 :: (Text, [Text], Game PlainWorldModel ())
ex21 = ("Escape", escapeTestMeWith, escapeWorld)

escapeWorld :: Game PlainWorldModel ()
escapeWorld = do
  setTitle "Escape"
  yb <- addRoom "Your Bedroom" ! done
  gs <- addRoom "Grassy Slope" ! done
  w <- addDoor "bedroom window"
    ! #front (yb, East)
    ! #back (gs, West)
    ! done
  insteadOf #searching [theObject w] $ \_ -> do
    bs <- getOtherSideOfDoor w
    [saying|Through the window, you make out {bs}.|]
  insteadOf #climbing [theObject w] $ tryAction "enter" [TheThing $ coerceTag w]
    -- Nothing <$ parseAction silentAction [] "open door"
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