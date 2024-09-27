module Yaifl.Test.Chapter3.Tamed where

import Yaifl.Prelude
import Yaifl.Game.EffectHandlers
import Yaifl (PlainWorldModel)
import Yaifl.Model.Metadata
import Yaifl.Game.Create.Object
import Yaifl.Model.Query
import Yaifl.Text.SayQQ
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.DynamicText
import Yaifl.Text.Say
import Yaifl.Game.ObjectSpecifics
import Yaifl.Model.Kinds.Device
import Yaifl.Model.Kinds.Person
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Actions.Args
import Yaifl.Model.Kinds (ThingPortable(..))
import Yaifl.Game.Create
import Yaifl.Model.Rules (parseAction)
import System.Random.Stateful

ex13 :: (Text, [Text], Game PlainWorldModel ())
ex13 = ("Tamed", tamedTestMeWith, tamedWorld)

tamedWorld :: Game PlainWorldModel ()
tamedWorld = do
  setTitle "Tamed"
  tcr <- addRoom "The Center Ring" ! done

  tc <- addContainer "cage"
    ! #enterable Enterable
    ! #openable Openable
    ! #opacity Transparent
    ! #opened Closed
    ! done
  l <- addThing "lion"
    ! #location (inThe tc)
    ! done
  ped <- addSupporter "pedestal"
    ! #enterable Enterable
    ! done
  p <- getPlayer
  p `isNowOn` ped
  everyTurn [whenIn tc] $ do
    r <- uniformM globalStdGen
    if r then
      [saying|The lion eyes you with obvious discontent.|]
      else [saying|Though the lion does not move, you are aware that it is watching you closely.|]
  tmb <- addContainer "magician's booth"
      ! #initialAppearance "Off to one side is a magician's booth, used in disappearing acts. The exterior is covered with painted gilt stars."
      ! #enterable Enterable
      ! #openable NotOpenable
      ! #modify (#objectData % #portability .~ FixedInPlace)
      ! done
  tsv <- addRoom "Starry Vastness"
      ! done
  tsv `isInsideFrom` tcr

  insteadOf (ActionRule #entering) [theObject tmb] $ \_ -> do
    Nothing <$ parseAction normalAction [] "in"
  pass

tamedTestMeWith :: [Text]
tamedTestMeWith = ["get in cage", "open cage", "get in cage", "z", "close cage", "out", "open cage", "get on pedestal",
  "get off", "look", "enter booth", "out"]
