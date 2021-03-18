module Yaifl.Components.Enclosing
(
    Enclosing(..)
  , encloses
  , enclosingCapacity
) where

import           Yaifl.Common
import           Yaifl.Prelude

data Enclosing = Enclosing
    {
        _encloses :: Set Entity
        , _enclosingCapacity :: Maybe Int
    } deriving Show

makeLenses ''Enclosing

