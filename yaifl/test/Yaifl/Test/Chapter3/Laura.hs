module Yaifl.Test.Chapter3.Laura where

import Yaifl.Prelude

import Yaifl (PlainWorldModel)

import Yaifl.Std.Create.Object
import Yaifl.Std.EffectHandlers
import Yaifl.Core.Metadata
import Yaifl.Test.Common
import Yaifl.Std.Kinds.Person
import Yaifl.Core.Query.Object (isUnderstoodAs)

ex19 :: (Text, [Text], Game PlainWorldModel ())
ex19 = ("Laura", lauraTestMeWith, lauraWorld)

lauraWorld :: Game PlainWorldModel ()
lauraWorld = do
  setTitle "Laura"
  addRoom "City of Angels" ! done

  -- this one is made very easy because we don't have natural english problems
  ip <- addThing "incriminating photograph of a woman with blonde hair" ! done
  dr <- addThing "drawing" ! done
  -- Understand "eyes" or "brown eyes" as "[brown eyes]". Understand "man" or "man with [brown eyes]" or "brown-eyed man" as "[man]". Understand "[man]" or "drawing of [man]" or "drawing of a [man]" as the drawing.]
  dr `isUnderstoodAs` (do
    brownEyes <- ["eyes", "brown eyes"]
    man <- ["man", "man with " <> brownEyes, "brown-eyed man"]
    [man, "drawing of " <> man, "drawing of a " <> man]
    )
  p <- getPlayer
  ip `isNowCarriedBy` p
  dr `isNowCarriedBy` p
  pass

-- okay, so technically "x of" should only get the incriminating photograph, because the drawing of the man is actually just called
-- 'drawing' with the printed name 'drawing of a man with brown eyes' and then the parser does not use the PRINTED name for deciding
-- which you mean. which, IMHO, is very silly. if an object is named "drawing of a man" and you accept "a" as a possible way to refer
-- to it, why not "of"?

-- I'm going to ignore this quirk.

lauraTestMeWith :: [Text]
lauraTestMeWith = fromI7TestMe [wrappedText|x photograph / x incriminating photograph of a woman with blonde hair
/ x hair / x blonde / x woman with blonde hair / x incriminating photograph of a woman / x drawing / x man / x of
/ x drawing of man / x drawing of a man / x drawing of a man with brown eyes / x drawing of a brown-eyed man / x brown eyes|]