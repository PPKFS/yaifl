module Yaifl.Properties.Openable where

import Solitude
import qualified Data.EnumSet as ES
import Yaifl.Common (Entity)

data Openable = Open | Closed deriving stock (Eq, Show)