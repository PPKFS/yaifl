module Yaifl.Components.Supporter
    ( Scenery(..)
    , supporterComponent
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Say
import Yaifl.Components.Object

newtype Scenery = Scenery ()

sceneryComponent :: Proxy Scenery
sceneryComponent = Proxy
