{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.Properties.Door
  ( Door(..)
  , blankDoor
  , getDoor
  ) where

import Yaifl.Core.Properties.TH

import Yaifl.Core.Objects.Query
import Yaifl.Core.Properties.Has
import Yaifl.Core.Properties.Query
import Yaifl.Core.Entity
import Solitude
import Effectful

data Door = Door
  { _backSide :: Entity
  , _isOneWay :: Bool
  } deriving stock (Eq, Show, Read)

blankDoor :: Entity -> Door
blankDoor = flip Door False

makeLenses ''Door
makeSpecificsWithout [] ''Door