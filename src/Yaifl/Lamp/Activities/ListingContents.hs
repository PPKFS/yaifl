module Yaifl.Lamp.Activities.ListingContents
( listingContentsImpl
, WithListingContents
) where

import Solitude

import Yaifl.Core.Actions.Activity
import Yaifl.Core.Object
import Yaifl.Core.Rules.Rule
import Yaifl.Lamp.Say
import Yaifl.Core.SayQQ
import Yaifl.Lamp.ListWriter
import Yaifl.Core.Rules.RuleEffects

type WithListingContents wm = (
  WithPrintingNameOfSomething wm
  , WithActivity "listingContents" wm (ListWritingParameters wm) ()
  )

listingContentsImpl ::
  Activity wm [AnyObject wm] ()
listingContentsImpl = makeActivity "Listing contents of something" [makeRule "standard listing contents" []
  (\objs -> do
    -- giving brief inventory information, tersely, not listing
    -- concealed items, listing marked items only;
    let objectsWithContents = (blankListWritingParameters objs)
          { asEnglishSentence = True
          , tersely = True
          , includingContents = True
          -- to avoid the infinite loop, this doesn't start the activity again
          , asListingActivity = False
          }
    [saying|{objectsWithContents}|]
    pure Nothing )]