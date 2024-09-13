module Yaifl.Test.Chapter3.Tamed where

import Yaifl.Prelude
import Yaifl.Game.EffectHandlers
import Yaifl (PlainWorldModel)
import Yaifl.Model.Metadata
import Yaifl.Test.Common
import Yaifl.Game.Create.Object
import Yaifl.Model.Query
import Named
import Yaifl.Text.SayQQ
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.DynamicText
import Yaifl.Text.Say
import Yaifl.Game.ObjectSpecifics
import Yaifl.Model.Kinds.Device
import Yaifl.Model.Kinds.Person

ex13 :: (Text, [Text], Game PlainWorldModel ())
ex13 = ("Tamed", tamedTestMeWith, tamedWorld)

tamedWorld :: Game PlainWorldModel ()
tamedWorld = do
  setTitle "Tamed"
  addRoom "Garden" ! done

  thp <- addDevice "holographic projector"
      ! #initialAppearance "The one major source of entertainment is the holographic projector, a top of the line Misthon 9000, on which you view every beam you can get."
      ! #description (text "projector description" $ withThing $ \t -> do
          let isOn = fromMaybe False $ getDeviceMaybe t ^? _Just % #switchedOn
          [sayingTell|{?if isOn}The projector is now playing a documentary about the early politics of the Mars colony.
{?else}The air above the projector is disappointingly clear.{?end if}|])
      ! done

  thp `isUnderstoodAs` ["holo", "holograph", "Misthon", "9000"]
  addPerson "Lewis"
    ! #gender Male
    ! #description "A wiry, excitable engineer who just signed aboard last week."
    ! done
  addPerson "Harper"
    ! #gender Male
    ! #description "Harper's a good guy: taciturn when sober, affectionate when drunk, but rarely annoying in either state."
    ! done
  "man" `kindIsUnderstoodAs` ["man", "guy", "chap", "lad", "male"]
  "man" `kindPluralIsUnderstoodAs` ["men", "guys", "chaps", "lads", "males"]
  pass

tamedTestMeWith :: [Text]
tamedTestMeWith = ["x holo", "x man", "lewis", "x guy", "harper", "turn on projector",  "x holo projector", "get men"]
