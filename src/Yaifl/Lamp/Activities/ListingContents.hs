module Yaifl.Lamp.Activities.ListingContents
( listingContentsImpl
, WithListingContents
) where

import Solitude

import Yaifl.Core.Actions.Activity
import Yaifl.Core.Object
import Yaifl.Core.Responses
import Yaifl.Core.Rules.Rule
import Yaifl.Lamp.Say

type WithListingContents wm = (
  WithPrintingNameOfSomething wm
  , WithResponse wm "youCanAlsoSeeA" ()
  , WithResponse wm "youCanAlsoSeeC" (AnyObject wm)
  , WithResponse wm "youCanAlsoSeeB" (AnyObject wm)
  , WithResponse wm "youCanAlsoSeeD" ()
  , WithResponse wm "youCanAlsoSeeE" ()
  , WithResponse wm "youCanAlsoSeeF" ()
  , WithActivity "listingContents" wm [AnyObject wm] ()
  )

listingContentsImpl ::
  Activity wm [AnyObject wm] ()
listingContentsImpl = makeActivity "Listing contents of something" [makeRule "standard listing contents" []
  (\_objs -> do
    pure Nothing)]