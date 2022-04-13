module Yaifl.Objects.Room where
import Yaifl.Directions
import Yaifl.Common
import Solitude
import Yaifl.Objects.ObjectData
import Yaifl.Objects.Object (ObjectLike(getRoom))

isWestOf ::
  WMHasDirections wm
  => MonadState (RoomData wm) m
  => Entity
  -> m ()


isDirectionFrom ::
  MonadState (RoomData wm) m
  => Entity
  -> m ()
isDirectionFrom e = do
  r <- getRoom e