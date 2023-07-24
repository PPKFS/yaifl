{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Model.Properties.Door
  ( Door(..)
  , blankDoor
  , getDoor
  ) where


import Solitude

import Yaifl.Model.Entity
import Yaifl.Model.Objects.Query
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH

data Door = Door
  { _backSide :: Entity
  , _isOneWay :: Bool
  } deriving stock (Eq, Show, Read)

blankDoor :: Entity -> Door
blankDoor = flip Door False

makeLenses ''Door
makeSpecificsWithout [] ''Door