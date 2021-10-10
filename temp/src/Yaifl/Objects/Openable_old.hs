module Yaifl.Components.Openable
  ( Openable (..),
  )
where

import Yaifl.Prelude

data Openable = Open | Closed deriving (Eq, Show)
