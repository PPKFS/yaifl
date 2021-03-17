module Yaifl.Components.Enclosing
(
    Enclosing(..),
    encloses
) where

import           Yaifl.Common
import           Yaifl.Prelude

newtype Enclosing = Enclosing
    {
        _encloses :: Set Entity
    } deriving Show
makeLenses ''Enclosing

