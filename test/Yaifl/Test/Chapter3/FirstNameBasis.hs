module Yaifl.Test.Chapter3.FirstNameBasis where

import Solitude
import Yaifl.Game.EffectHandlers
import Yaifl (PlainWorldModel)
import Yaifl.Model.Metadata
import Yaifl.Test.Common
import Yaifl.Game.Create.Object
import Yaifl.Model.Query
import Named ((!))

ex11 :: (Text, [Text], Game PlainWorldModel ())
ex11 = ("First Name Basis", firstNameBasisTestMeWith, firstNameBasisWorld)

firstNameBasisWorld :: Game PlainWorldModel ()
firstNameBasisWorld = do
  setTitle "First Name Basis"
  tcl <- addRoom "The Crew Lounge" [wrappedText|Deliberately spartan: the crew feels weight restrictions here first,
so there aren't any chairs, just a few thin pads on the ground.|]
  thp <- addDevice "holographic projector" "The one major source of entertainment is the holographic projector, a top of the line Misthon 9000, on which you view every beam you can get." !
    #description ()"[if switched on]The projector is now playing a documentary about the early politics of the Mars colony.[otherwise]The air above the projector is disappointingly clear.[end if]"
  thp `isUnderstoodAs` ["holo", "holograph", "Misthon", "9000"]
  lewis <- addPerson "Lewis" Male

  pass

firstNameBasisTestMeWith :: [Text]
firstNameBasisTestMeWith = []
