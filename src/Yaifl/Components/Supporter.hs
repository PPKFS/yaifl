module Yaifl.Components.Supporter
    ( Supporter(..)
    , supporterComponent
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Say
import Yaifl.Components.Object

data Supporter = Supporter
    {
        _supporterEnterable :: Bool,
        _supporterCarryingCapacity :: Int
    } deriving Show
supporterComponent :: Proxy Supporter
supporterComponent  = Proxy :: (Proxy Supporter)