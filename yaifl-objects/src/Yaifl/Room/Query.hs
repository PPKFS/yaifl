module Yaifl.Room.Query
  ( getAllObjectsInRoom
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.Enclosing.Kind
import Yaifl.Room.Kind
import Yaifl.Enclosing.Query
import Yaifl.Thing.Kind
import Yaifl.ObjectLike
import Yaifl.Tag
import Yaifl.Property.Has


getAllObjectsInRoom ::
  RoomLike wm o
  => WMWithProperty wm Enclosing
  => WithoutMissingObjects wm es
  => IncludeScenery
  -> IncludeDoors
  -> RecurseAllObjects
  -> o
  -> Eff es [Thing wm]
getAllObjectsInRoom incScenery incDoors recurse r = do
  r' <- getRoom r
  getAllObjectsInEnclosing incScenery incDoors recurse (coerceTag $ tagRoomEntity r')
