module Yaifl.Components.Openable
    ( Openable(..)
    , openableComponent
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Say
import Yaifl.Components.Object

data Openable = Open | Closed deriving (Eq, Show)
openableComponent :: Proxy Openable
openableComponent = Proxy :: (Proxy Openable)