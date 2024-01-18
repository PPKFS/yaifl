module Yaifl.Game.Activities.ListingContents
( listingContentsImpl
, WithListingContents
) where

import Solitude

import Yaifl.Model.Activity
import Yaifl.Model.Rules.Rulebook
import Yaifl.Text.ListWriter
import Effectful.Writer.Static.Local (execWriter)
import Yaifl.Text.Say
import Yaifl.Text.Print

type WithListingContents wm = (
  WithListWriting wm
  )

listingContentsImpl :: WithListingContents wm => Activity wm () (ListWritingParameters wm) ()
listingContentsImpl = makeActivity "Listing contents of something" [makeRule "standard listing contents" []
  (\objs -> do
    -- giving brief inventory information, tersely, not listing
    -- concealed items, listing marked items only;
    let objectsWithContents = objs
          { asEnglishSentence = True
          , tersely = True
          , includingContents = True
          -- to avoid the infinite loop, this doesn't start the activity again
          , asListingActivity = False
          }
    execWriter (writeListOfThings objectsWithContents) >>= say >> runOnParagraph
    pure Nothing )]