module Yaifl.Lamp.ListWriter
  ( ListOfObjects(..)
  , withContents

  ) where

import Yaifl.Core.Object
import Solitude
import Yaifl.Core.Rules.RuleEffects
import Effectful.Writer.Static.Local ( tell )

data ListOfObjects wm = ListOfObjects
  { objects :: [AnyObject wm]
  -- whether this should invokve the "listing contents of something" activity
  , asListingActivity :: Bool
  -- whether this should list the contents of each item
  , listContents :: Bool
  }

instance SayableValue (ListOfObjects wm) wm where
  sayTell _ = tell ""

withContents ::
  [AnyObject wm]
  -> ListOfObjects wm
withContents o = ListOfObjects o False True