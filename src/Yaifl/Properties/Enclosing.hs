module Yaifl.Properties.Enclosing where

import Solitude
import qualified Data.EnumSet as ES
import Yaifl.Common (Entity)

data Enclosing = Enclosing
  { _enclosingContains :: ES.EnumSet Entity
  , _enclosingCapacity :: Maybe Int
  } deriving stock (Show, Eq)