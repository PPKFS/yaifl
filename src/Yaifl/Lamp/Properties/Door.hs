{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.Properties.Door
  ( Door(..)
  , blankDoor
  , getDoor
  ) where


import Solitude

import Yaifl.Core.Entity
import Yaifl.Core.Objects.Query
import Yaifl.Core.Properties.Has
import Yaifl.Core.Properties.Query
import Yaifl.Core.Properties.TH

data Door = Door
  { _backSide :: Entity
  , _isOneWay :: Bool
  } deriving stock (Eq, Show, Read)

blankDoor :: Entity -> Door
blankDoor = flip Door False

makeLenses ''Door
makeSpecificsWithout [] ''Door