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
  , WithActivity "listingContents" wm (ListWritingVariables wm) ()
  )

listingContentsImpl ::
  Activity wm [AnyObject wm] ()
listingContentsImpl = makeActivity "Listing contents of something" [makeRule "standard listing contents" []
  (\objs -> do
    -- giving brief inventory information, tersely, not listing
    -- concealed items, listing marked items only;
    let objectsWithContents = ListWritingVariables
          { objects = objs
          , listConcealedThings = False
          , tersely = True
          , listContents = True
          , asListingActivity = True
          }
    [saying|{objectsWithContents}|]
    pure Nothing )]