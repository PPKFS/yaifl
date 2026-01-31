module Yaifl.Chapter3.StarryVoid where

import Yaifl.Prelude

import Yaifl
import Yaifl.Entity
import Yaifl.ObjectLike
import Yaifl.Actions.Going
import Yaifl.Actions.Args
import Yaifl.Direction.Kind
import Yaifl.Openable.Kind
import Yaifl.Object.Query
import Yaifl.Text.SayableValue
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.DynamicText
import Yaifl.Text.Say
import Yaifl.Effects.RuleEffects
import Yaifl.Room.Create
import Yaifl.Object.Create
import Yaifl.Room.Query
import Yaifl.Door.Create
import Yaifl.Person.Query
import Yaifl.Thing.Query
import Yaifl.Preconditions
import Yaifl.Create.Rule

ex7 :: (Text, [Text], Game PlainWorldModel ())
ex7 = ("Starry Void", starryVoidTestMeWith, starryVoidWorld)

starryVoidWorld :: Game PlainWorldModel ()
starryVoidWorld = do
  setTitle "Starry Void"
  tcr <- addRoom "The Centre Ring"
    ! done
  tsv <- addRoom "The Starry Void"
    ! done
  tsv `isInsideFrom` tcr

  tmb <- addDoor $ newDoor
    { name = "magician's booth"
    , initialAppearance = DynamicText @PlainWorldModel $ Right ("description of magician's booth door", RuleLimitedEffect $
        withThing $ \t ->
        do
          p <- getPlayer
          picr <- (== unTagEntity tcr) . getID <$> getLocation p
          let cl = isClosed t
          [sayingTell|{?if picr}A magician's booth stands in the corner, painted dark blue with glittering gold stars.¬
      {?else if cl}A crack of light indicates the way back out to the center ring.¬
      {?else}The door stands open to the outside.{?end if}|])
    , front = (tsv, Out)
    , back = (tcr, In)
    }

  insteadOf #examining [theObject tmb, whenIn tcr] $ \_ -> do
    booth <- getObject tmb
    let cl = isOpen booth
    [saying|It is dark blue and glittering with gold stars. {?if cl}The door currently stands open{?else}It has been firmly shut{?end if}.|]

  insteadOf #examining [theObject tmb, whenIn tsv] $ \_ -> do
    booth <- getObject tmb
    let cl = isOpen booth
    [saying|The booth door is {?if cl}wide open{?else}shut, admitting only a thin crack of light{?end if}.|]

  tmb `isUnderstoodAs` ["door", "of", "the", "light", "crack", "thin crack"]

  before #going [throughTheClosedDoor tmb] "" $ \_ -> do
    [saying|(first opening the door of the booth)|]
    Nothing <$ parseAction silentAction [] "open door"
  pass

starryVoidTestMeWith :: [Text]
starryVoidTestMeWith = ["examine booth", "open door of the booth", "in", "examine door", "close door", "look", "examine crack of light"]