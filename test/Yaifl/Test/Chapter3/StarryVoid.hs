module Yaifl.Test.Chapter3.StarryVoid where


import Solitude
import Yaifl
import Yaifl.Metadata
import Yaifl.Model.Object
import Yaifl.Model.ObjectSpecifics
import Yaifl.Model.Objects.Create
import Yaifl.Model.Objects.Query
import Yaifl.Model.Objects.RoomConnections
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Openable
import Yaifl.Rules.Rule
import Yaifl.Rules.RuleEffects
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.DynamicText
import Yaifl.Text.SayQQ
import Yaifl.Model.Properties.Door
import Yaifl.Model.Direction
import Yaifl.Rules.Args (getPlayer)
import Yaifl.Rules.Adding
import Yaifl.Actions.Going

boothDesc :: WMWithProperty wm Openable => Room wm -> DynamicText wm
boothDesc tcr = DynamicText $ Right ("description of magician's booth door", RuleLimitedEffect $
  withThing $ \t ->
  do
    p <- getPlayer
    picr <- (== tcr) <$> getLocation p
    let cl = isClosed t
    [sayingTell|{?if picr}A magician's booth stands in the corner, painted dark blue with glittering gold stars.¬
{?else if cl}A crack of light indicates the way back out to the center ring.¬
{?else}The door stands open to the outside.{?end if}|])

starryVoidWorld :: Game PlainWorldModel ()
starryVoidWorld = do
  setTitle "Starry Void"
  tcr <- addRoom "The Centre Ring" ""
  tsv <- addRoom "The Starry Void" ""
  tsv `isInsideFrom` tcr
  tmb <- addDoor "The magician's booth" (boothDesc tcr) "" (tsv, Out) (tcr, In) Nothing

  insteadOf (ActionRule #examining) [theObject tmb, whenIn tcr] "" $ \_ -> do
    [sayingParagraph|It is dark blue and glittering with gold stars. [if the booth is open]The door currently stands open[otherwise]It has been firmly shut[end if].|]

  insteadOf (ActionRule #examining) [theObject tmb, whenIn tsv] "" $ \_ -> do
    [sayingParagraph|The booth door is [if the magician's booth is open]wide open[otherwise]shut, admitting only a thin crack of light[end if].|]

  --tmb `isUnderstoodAs` ["door", "of", "the", "light", "crack", "thin crack"]
  before (ActionRule #going) [throughTheDoor tmb] "" $ \_ -> do
    --say "(first opening the door of the booth)[command clarification break]"
    say ("silently try opening the booth." :: Text)
    rulePass
  pass

starryVoidTestMeWith :: [Text]
starryVoidTestMeWith = ["examine booth", "open door of the booth", "in", "examine door", "close door", "look", "examine crack of light"]