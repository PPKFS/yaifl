module Yaifl.Test.Chapter3.Tamed where

import Yaifl.Prelude
import Yaifl.Game.EffectHandlers
import Yaifl (PlainWorldModel)
import Yaifl.Core.Metadata
import Yaifl.Game.Create.Object
import Yaifl.Model.Query
import Yaifl.Text.SayQQ
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.DynamicText
import Yaifl.Text.Say
import Yaifl.Game.ObjectSpecifics
import Yaifl.Model.Kinds.Device
import Yaifl.Game.Create.RoomConnection
import Yaifl.Model.Kinds.Container
import Yaifl.Model.Kinds.Openable
import Yaifl.Model.Actions.Args
import Yaifl.Model.Kinds (ThingPortable(..))
import Yaifl.Game.Create
import Yaifl.Model.Rules (parseAction)

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
  tmb <- addContainer "magician's booth"
      ! #initialAppearance "Off to one side is a magician's booth, used in disappearing acts. The exterior is covered with painted gilt stars."
      ! #enterable Enterable
      ! #openable NotOpenable
      ! #portable FixedInPlace
      ! done
  tsv <- addRoom "Starry Vastness"
      ! done
  tsv `isInsideFrom` tcr
{-}
  insteadOf (ActionRule #entering) [theObject tmb] $ \_ -> do
    Nothing <$ parseAction normalAction [] "in"-}
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
