module Yaifl.Test.Chapter3.Garibaldi
  ( ex22
  ) where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Object.Kind
import Yaifl.Std.Create.Object
import Yaifl.Std.EffectHandlers
import Yaifl.Std.ObjectSpecifics
import Yaifl.Core.Metadata
import Yaifl.Test.Common
import Yaifl.Std.Kinds.Direction
import Yaifl.Std.Create
import Yaifl.Text.SayableValue
import Yaifl.Std.Actions.Imports
import Yaifl.Std.Actions.Examining
import Yaifl.Entity
import Yaifl.Core.Effects
import Yaifl.Std.Kinds.Door
import Yaifl.Std.Kinds.Openable
import Yaifl.Std.Kinds.Person
import Yaifl.Object.Create

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
            locked = isLocked door
        [saying|#{linebreak} {door} ({frontSide}/{backSide}): {?if locked}LOCKED{?else}UNLOCKED{?end if} |]
      rulePass
    [saying|#{paragraphBreak}|]
  p <- getPlayer
  sr `isNowCarriedBy` p

  tdb <- addRoom "Docking Bay" ! #modify makeNameImproper ! done
  tz <- addRoom "Zocalo" ! #modify makeNameImproper ! done
  s <- addRoom' "Space" ! done
  ml <- addRoom "Medlab" ! done
  tia <- addDoor "inner airlock"
          ! #frontSide northOf tdb
          ! #backSide southOf tz
          ! #modify makeUnlocked
          ! done
  pass

whenSwitchedOn :: ThingEntity -> Precondition PlainWorldModel (Args PlainWorldModel (ExaminingActionVariables PlainWorldModel))
whenSwitchedOn = error ""

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