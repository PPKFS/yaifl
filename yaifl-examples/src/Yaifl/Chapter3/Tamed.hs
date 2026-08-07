module Yaifl.Chapter3.Tamed where


import Yaifl.Prelude

import Yaifl (PlainWorldModel)
import Yaifl.Thing.Kind
import Yaifl.Metadata
import Yaifl.Effects.Interpreters
import Yaifl.Container.Kind
import Yaifl.Openable.Kind
import Yaifl.Text.Say
import Yaifl.Container.Create
import Yaifl.Object.Kind
import Yaifl.Room.Create
import Yaifl.Thing.Create
import Yaifl.Supporter.Create
import Yaifl.Person.Query
import Yaifl.Create.Rule
import Yaifl.Supporter.Query
import Yaifl.Preconditions
import Yaifl.Room.Query
import Yaifl.Combinators

ex13 :: (Text, [Text], WorldConstruction PlainWorldModel ())
ex13 = ("Tamed", tamedTestMeWith, tamedWorld)

tamedWorld :: WorldConstruction PlainWorldModel ()
tamedWorld = do
  setTitle "Tamed"
  tcr <- addRoom' "Center Ring"

  tc <- addContainer "cage" $ newContainer
    & makeItEnterable
    & makeItClosedAndOpenable
    & #opacity .~ Transparent

  addThing "lion" $ newThing
    & placeIt (inThe tc)

  ped <- addSupporter "pedestal" $ newSupporter
    & makeItEnterable

  p <- getPlayer
  p `isNowOn` ped
  everyTurn "check the lion rule" [whenPlayerIsIn tc] $ do
    r <- random @Bool
    if r then
      [saying|The lion eyes you with obvious discontent.|]
      else [saying|Though the lion does not move, you are aware that it is watching you closely.|]
  addContainer "magician's booth" $ newContainer
    & #initialAppearance .~ "Off to one side is a magician's booth, used in disappearing acts. The exterior is covered with painted gilt stars."
    & makeItEnterable
    & #openStatus % _2 .~ NotOpenable
    & #thingModify %~ (>> (#objectData % #portable) .= FixedInPlace)
  tsv <- addRoom' "Starry Vastness"
  tsv `isInsideFrom` tcr
  pass

tamedTestMeWith :: [Text]
tamedTestMeWith = [
  -- (on the pedestal)
  "get in cage", "open cage",
  -- (getting off the pedestal)
  "get in cage", "z", "close cage",
  -- You can't get out of the closed cage.
  "out", "open cage",
  -- (getting out of the cage)
  "get on pedestal",
  "get off", "look", "enter booth", "out"]
