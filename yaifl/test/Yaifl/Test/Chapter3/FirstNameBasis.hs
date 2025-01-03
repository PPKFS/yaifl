module Yaifl.Test.Chapter3.FirstNameBasis where

import Yaifl.Prelude

import Yaifl
import Yaifl.Std.Create
import Yaifl.Core.Query.Object
import Yaifl.Text.AdaptiveNarrative
import Yaifl.Text.DynamicText
import Yaifl.Text.Say
import Yaifl.Std.Kinds.Device
import Yaifl.Std.Kinds.Person

ex11 :: (Text, [Text], Game PlainWorldModel ())
ex11 = ("First Name Basis", firstNameBasisTestMeWith, firstNameBasisWorld)

firstNameBasisWorld :: Game PlainWorldModel ()
firstNameBasisWorld = do
  setTitle "First Name Basis"
  addRoom "The Crew Lounge"
    ! #description "Deliberately spartan: the crew feels weight restrictions here first, so there aren't any chairs, just a few thin pads on the ground."
    ! done

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

firstNameBasisTestMeWith :: [Text]
firstNameBasisTestMeWith = ["x holo", "x man", "lewis", "x guy", "harper", "turn on projector",  "x holo projector", "get men"]
