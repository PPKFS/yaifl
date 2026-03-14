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
import Yaifl.Object.Create
import Yaifl.Room.Create
import Yaifl.Thing.Create
import Yaifl.Supporter.Create
import Yaifl.Person.Query
import Yaifl.Create.Rule
import Yaifl.Supporter.Query
import Yaifl.Preconditions
import Yaifl.Room.Query

ex13 :: (Text, [Text], Game PlainWorldModel ())
ex13 = ("Tamed", tamedTestMeWith, tamedWorld)

tamedWorld :: Game PlainWorldModel ()
tamedWorld = do
  setTitle "Tamed"
  tcr <- addRoom "Center Ring" ! done

  tc <- addContainer "cage"
    ! #enterable Enterable
    ! #openable Openable
    ! #opacity Transparent
    ! #opened Closed
    ! done
  addThing "lion"
    ! #location (inThe tc)
    ! done
  ped <- addSupporter "pedestal"
    ! #enterable Enterable
    ! done
  p <- getPlayer
  p `isNowOn` ped
  everyTurn "" [whenPlayerIsIn tc] $ do
    r <- random @Bool
    if r then
      [saying|The lion eyes you with obvious discontent.|]
      else [saying|Though the lion does not move, you are aware that it is watching you closely.|]
  _tmb <- addContainer "magician's booth"
      ! #initialAppearance "Off to one side is a magician's booth, used in disappearing acts. The exterior is covered with painted gilt stars."
      ! #enterable Enterable
      ! #openable NotOpenable
      ! #portable FixedInPlace
      ! done
  tsv <- addRoom "Starry Vastness"
      ! done
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
