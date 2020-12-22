module Yaifl.Components.Enclosing
(
    Enclosing(..),
    enclosingComponent
) where

import           Yaifl.Common2
import           Yaifl.Say2
import           Yaifl.Prelude
import           Yaifl.World
import Yaifl.Components.Object

newtype Enclosing = Enclosing
    {
        _encloses :: Set Entity
    } deriving Show
makeLenses ''Enclosing

enclosingComponent :: Proxy Enclosing
enclosingComponent = Proxy
