module Yaifl.Door.Query
  ( getDoor
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.TH
import Yaifl.Tag

import Yaifl.ObjectLike
import Yaifl.Door.Kind
import Yaifl.Object.Kind
import Yaifl.Person.Query

getDoor ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Door
  => DoorEntity
  -> Eff es Door
getDoor de = do
  t <- getThing (coerceTag @ThingTag de)
  return $ fromMaybe (error "property witness violated") $ getDoorMaybe t

getOtherSideOfDoor ::
  WithoutMissingObjects wm es
  => WMWithProperty wm Door
  => DoorEntity
  -> Eff es RoomEntity
getOtherSideOfDoor door = do
  p <- getPlayerLocation
  d <- getDoor door
  return $ if p `objectEquals` (d ^. #frontSide) then d ^. #backSide else d ^. #frontSide