module Yaifl.Chapter3.Escape
  ( ex21
  ) where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Object.Kind
import Yaifl.Effects.Interpreters
import Yaifl.Metadata
import Yaifl.Test.Common
import Yaifl.Direction.Kind
import Yaifl.Text.SayableValue
import Yaifl.Actions.Imports
import Yaifl.Actions.Going
import Yaifl.Room.Create
import Yaifl.Door.Create
import Yaifl.Create.Rule
import Yaifl.Door.Query
import Yaifl.Preconditions
import Yaifl.Combinators

ex21 :: (Text, [Text], Game PlainWorldModel ())
ex21 = ("Escape", escapeTestMeWith, escapeWorld)

escapeWorld :: Game PlainWorldModel ()
escapeWorld = do
  setTitle "Escape"
  yb <- addRoom' "Your Bedroom"
  gs <- addRoom "Grassy Slope" $ newRoom & makeRoomImproperlyNamed
  w <- addDoor "bedroom window" $ newDoor (yb `isToThe` West) (gs `isToThe` East)

  insteadOf' #searching [theObject w] $ do
    bs <- getOtherSideOfDoor w
    [saying|Through the window, you make out {the bs}.|]
  insteadOf #climbing [theObject w] $ tryActionWithThing "enter" w

  -- the original requires you to define "climb through [something]" as an alias, whereas
  -- my parser will just assume you want to climb something called the "through window" and considers
  -- one word enough of a match.
  -- I don't know if this needs fixing but if I leave this here for when I inevitably rewrite the parser it'll help.
  insteadOf' #going [throughTheClosedDoor w] [saying|The window is shut: you'd break the glass.|]

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