module Yaifl.Test.Chapter3.Garibaldi
  ( ex22
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
import Yaifl.Std.Actions.Examining
import Yaifl.Core.Entity
import Yaifl.Core.Effects
import Yaifl.Std.Kinds.Door


ex22 :: (Text, [Text], Game PlainWorldModel ())
ex22 = ("Garibaldi", escapeTestMeWith, garibaldiWorld)

garibaldiWorld :: Game PlainWorldModel ()
garibaldiWorld = do
  setTitle "Garibaldi"
  sr <- addDevice "security readout"
    ! #initialAppearance "The one major source of entertainment is the holographic projector, a top of the line Misthon 9000, on which you view every beam you can get."
    ! #description "The screen is blank."
    ! done
  insteadOf #examining [theObject sr, whenSwitchedOn sr] $ \_ -> do
    [saying|The screen reads:#{linebreak}|] -- missing a "[fixed letter spacing]"
    traverseThings_ $ \thing -> do
      whenJust (getDoorMaybe thing) $ \door -> do
        let frontSide = door ^. #frontSide
            backSide = door ^. #backSide
            locked = door ^. #opened % #lockability % #locked
        [saying|#{linebreak} {door} ({frontSide}/{backSide}): {?if locked} {?else} {?end if} |]
        error ""
      rulePass
  insteadOf #climbing [theObject w] $ tryAction "enter" [TheThing $ coerceTag w]

  -- the original requires you to define "climb through [something]" as an alias, whereas
  -- my parser will just assume you want to climb something called the "through window" and considers
  -- one word enough of a match.

  -- I don't know if this needs fixing but if I leave this here for when I inevitably rewrite the parser it'll help.
  insteadOf #going [throughTheClosedDoor w] $ const [saying|The window is shut: you'd break the glass.|]
  pass

whenSwitchedOn :: ThingEntity -> Precondition PlainWorldModel (Args PlainWorldModel (ExaminingActionVariables PlainWorldModel))
whenSwitchedOn = _

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