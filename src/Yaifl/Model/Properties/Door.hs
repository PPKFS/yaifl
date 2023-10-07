{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Model.Properties.Door
  ( DoorSpecifics(..)
  , blankDoor
  , getDoorSpecifics
  ) where


import Solitude

import Yaifl.Model.Entity
import Yaifl.Model.Objects.Query
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH
import Yaifl.Model.Objects.Effects

data DoorSpecifics = Door
  { backSide :: Entity
  , isOneWay :: Bool
  } deriving stock (Eq, Show, Read)

blankDoor :: Entity -> DoorSpecifics
blankDoor = flip Door False

makeFieldLabelsNoPrefix ''DoorSpecifics
makeSpecificsWithout [] ''DoorSpecifics