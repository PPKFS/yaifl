{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Model.Properties.Door
  ( DoorSpecifics(..)
  , blankDoorSpecifics
  , getDoorSpecificsMaybe
  , isOpen
  , isClosed
  ) where


import Solitude

import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Properties.Openable
import Yaifl.Model.Entity
import Yaifl.Model.Object

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
  WMWithProperty wm Openable
  => CanBeAny wm o
  => o
  -> Bool
isClosed o = Just Closed == getOpenableMaybe o

isOpen ::
  WMWithProperty wm Openable
  => CanBeAny wm o
  => o
  -> Bool
isOpen o = Just Open == getOpenableMaybe o
instance Taggable DoorSpecifics DoorTag

{-}

type DoorLike wm o = PropertyLike wm DoorSpecifics o


instance PropertyLike wm DoorSpecifics DoorEntity where
 getAs o = do
    a <- getObject o
    e <- getDoorSpecificsMaybe a
    getPropertyOrThrow "door" a e
    -}