module Yaifl.Components.Container
    ( Opacity(..)
    , Container(..)
    )
where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Say
import Yaifl.Components.Object

data Opacity = Opaque | Transparent deriving (Eq, Show)

data Container = Container
    {
        _enterable :: Bool,
        _opacity :: Opacity,
        _carryingCapacity :: Int
    } deriving Show
containerComponent :: Proxy Container
containerComponent = Proxy :: (Proxy Container)