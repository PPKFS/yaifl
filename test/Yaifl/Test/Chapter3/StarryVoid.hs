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
import Yaifl.Rules.Args
import Yaifl.Rules.Rule
import Yaifl.Rules.RuleEffects
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.DynamicText
import Yaifl.Text.SayQQ

boothDesc :: WMHasProperty wm Openable => Room wm -> DynamicText wm
boothDesc tcr = DynamicText $ Right ("description of magician's booth door", RuleLimitedEffect $ withThing $ \t -> do
  player <- getPlayer
  picr <- (== tcr) <$> getLocation player
  cl <- isClosed t
  [sayingTell|{?if picr}A magician's booth stands in the corner, painted dark blue with glittering gold stars.
  {?else if cl} A crack of light indicates the way back out to the center ring.
  {?else} The door stands open to the outside.{?end if}|]
  )

starryVoid :: Game PlainWorldModel ()
starryVoid = do
  setTitle "Starry Void"
  tcr <- addRoom "The Centre Ring" ""
  tsv <- addRoom "The Starry Void" ""
  tmb <- addDoor "The magician's booth" (Just $ boothDesc tcr) tcr tsv Nothing
  tcr `isInsideFrom` tsv
  --addDoorToConnection tmb (Out, tcr) (In, tsv)
  pass