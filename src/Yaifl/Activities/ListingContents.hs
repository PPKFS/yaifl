module Yaifl.Activities.ListingContents
( listingContentsImpl
, WithListingContents
) where

import Solitude

import Yaifl.Activities.Activity
import Yaifl.Rules.Rule
import Yaifl.Text.ListWriter
import Yaifl.Text.Print
import Effectful.Writer.Static.Local (execWriter)

type WithListingContents wm = (
  WithListWriting wm
  )

listingContentsImpl :: WithListingContents wm => Activity wm (ListWritingParameters wm) ()
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
    execWriter (writeListOfThings objectsWithContents) >>= printText
    pure Nothing )]