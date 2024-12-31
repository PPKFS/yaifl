module Yaifl.Test.Chapter3.StarryVoid where

import Yaifl.Prelude

import Yaifl
import Yaifl.Core.Entity
import Yaifl.Core.ObjectLike
import Yaifl.Core.Query.Enclosing
import Yaifl.Game.Actions.Going
import Yaifl.Game.Create
import Yaifl.Model.Actions.Args
import Yaifl.Model.Kinds.Direction
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Query
import Yaifl.Model.Rules.RuleEffects
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.DynamicText
import Yaifl.Text.Say

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

  tmb <- addDoor "magician's booth"
    ! #initialAppearance (DynamicText @PlainWorldModel $ Right ("description of magician's booth door", RuleLimitedEffect $
        withThing $ \t ->
        do
          p <- getPlayer
          picr <- (== unTag tcr) . getID <$> getLocation p
          let cl = isClosed t
          [sayingTell|{?if picr}A magician's booth stands in the corner, painted dark blue with glittering gold stars.¬
      {?else if cl}A crack of light indicates the way back out to the center ring.¬
      {?else}The door stands open to the outside.{?end if}|]))
    ! #front (tsv, Out)
    ! #back (tcr, In)
    ! done

  insteadOf (ActionRule #examining) [theObject tmb, whenIn tcr] $ \_ -> do
    booth <- getObject tmb
    let cl = isOpen booth
    [saying|It is dark blue and glittering with gold stars. {?if cl}The door currently stands open{?else}It has been firmly shut{?end if}.|]

  insteadOf (ActionRule #examining) [theObject tmb, whenIn tsv] $ \_ -> do
    booth <- getObject tmb
    let cl = isOpen booth
    [saying|The booth door is {?if cl}wide open{?else}shut, admitting only a thin crack of light{?end if}.|]

  tmb `isUnderstoodAs` ["door", "of", "the", "light", "crack", "thin crack"]

  before (ActionRule #going) [throughTheClosedDoor tmb] "" $ \_ -> do
    [saying|(first opening the door of the booth)|]
    Nothing <$ parseAction silentAction [] "open door"
  pass

starryVoidTestMeWith :: [Text]
starryVoidTestMeWith = ["examine booth", "open door of the booth", "in", "examine door", "close door", "look", "examine crack of light"]