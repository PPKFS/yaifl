module Yaifl.Components.Enclosing
(
    Enclosing(..),
    enclosingComponent
) where

import           Yaifl.Common
import           Yaifl.Prelude

newtype Enclosing = Enclosing
    {
        _encloses :: Set Entity
    } deriving Show
makeLenses ''Enclosing

enclosingComponent :: Proxy Enclosing
enclosingComponent = Proxy
