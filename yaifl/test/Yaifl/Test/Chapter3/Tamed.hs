module Yaifl.Test.Chapter3.Tamed where


import Yaifl.Prelude

import Yaifl (PlainWorldModel)
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Metadata
import Yaifl.Game.Create
import Yaifl.Game.EffectHandlers
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Openable
import Yaifl.Text.Say
import Yaifl.Model.Kinds.Person

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
  _l <- addThing "lion"
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
