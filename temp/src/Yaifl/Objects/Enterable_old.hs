module Yaifl.Components.Enterable
  ( Enterable (..),
  )
where

import Yaifl.Prelude

data Enterable = Enterable | NotEnterable deriving (Eq, Show)
