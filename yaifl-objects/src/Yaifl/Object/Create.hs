module Yaifl.Object.Create
  ( makeNameImproper
  , makeNameProper

  ) where

import Yaifl.Prelude
import Yaifl.Entity
import Yaifl.WorldModel (WMText)
import Yaifl.Object.Kind

makeNameImproper :: (WithLabel "nameProperness" NameProperness x, State x :> es) => Eff es ()
makeNameImproper = #nameProperness .= Improper

makeNameProper :: (WithLabel "nameProperness" NameProperness x, State x :> es) => Eff es ()
makeNameProper = #nameProperness .= Proper