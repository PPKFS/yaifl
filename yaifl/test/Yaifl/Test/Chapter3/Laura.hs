module Yaifl.Test.Chapter3.Laura where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Game.Create.Object
import Yaifl.Game.EffectHandlers
import Yaifl.Core.Metadata
import Yaifl.Test.Common
import Yaifl.Model.Kinds.Person

ex19 :: (Text, [Text], Game PlainWorldModel ())
ex19 = ("Laura", lauraTestMeWith, lauraWorld)

lauraWorld :: Game PlainWorldModel ()
lauraWorld = do
  setTitle "Laura"
  addRoom "City of Angels" ! done

  -- this one is made very easy because we don't have natural english problems
  ip <- addThing "incriminating photograph of a woman with blonde hair" ! done
  dr <- addThing "drawing of a man with brown eyes" !
    #modify (do
      pass
      )
    ! done
  p <- getPlayer
  ip `isNowCarriedBy` p
  dr `isNowCarriedBy` p
  pass

lauraTestMeWith :: [Text]
lauraTestMeWith = fromI7TestMe [wrappedText|x photograph / x incriminating photograph of a woman with blonde hair
/ x hair / x blonde / x woman with blonde hair / x incriminating photograph of a woman / x drawing / x man / x of
/ x drawing of man / x drawing of a man / x drawing of a man with brown eyes / x drawing of a brown-eyed man / x brown eyes|]