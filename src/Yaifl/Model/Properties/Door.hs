{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Model.Properties.Door
  ( DoorSpecifics(..)
  , DoorLike
  , blankDoorSpecifics
  , getDoorSpecificsMaybe
  , isOpen
  , isClosed
  ) where


import Solitude

import Yaifl.Model.Objects.Query
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Properties.Openable
import Yaifl.Model.Entity

data DoorSpecifics = Door
  { isOneWay :: Bool
  , openable :: Openable
  , frontSide :: RoomEntity
  , backSide :: RoomEntity
  } deriving stock (Eq, Show, Read)

blankDoorSpecifics :: RoomEntity -> RoomEntity -> DoorSpecifics
blankDoorSpecifics = Door False Closed

makeFieldLabelsNoPrefix ''DoorSpecifics
makeSpecificsWithout [] ''DoorSpecifics

isClosed ::
  NoMissingRead wm es
  => WMHasProperty wm Openable
  => ObjectLike wm o
  => o
  -> Eff es Bool
isClosed o = (Just Closed ==) <$> getOpenableMaybe o

isOpen ::
  NoMissingRead wm es
  => WMHasProperty wm Openable
  => ObjectLike wm o
  => o
  -> Eff es Bool
isOpen o = (Just Open ==) <$> getOpenableMaybe o


type DoorLike wm o = PropertyLike wm DoorSpecifics o
