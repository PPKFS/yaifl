module Yaifl.Properties.Container where

import Solitude
import qualified Data.EnumSet as ES
import Yaifl.Common (Entity)
import Yaifl.Properties.Enclosing
import Yaifl.Properties.Openable

data Opacity = Opaque | Transparent deriving stock (Eq, Show)
data Enterable = Enterable | NotEnterable deriving stock (Eq, Show)

data Container = Container
  { _containerOpacity :: Opacity
  , _containerEnclosing :: Enclosing
  , _containerOpenable :: Openable
  , _containerEnterable :: Enterable
  } deriving stock (Eq, Show)
  