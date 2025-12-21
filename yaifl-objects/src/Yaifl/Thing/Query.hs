module Yaifl.Thing.Query
  ( getLocation

  ) where

import Yaifl.Prelude



import Yaifl.Effects.ObjectQuery
import Yaifl.AnyObject
import Yaifl.Object.Kind
import Yaifl.Room.Kind
import Yaifl.ObjectLike


getLocation ::
  WithoutMissingObjects wm es
  => ThingLike wm o
  => o
  -> Eff es (Room wm)
getLocation t = do
  t' <- getThing t
  o <- getObject (t' ^. #objectData % #containedBy)
  asThingOrRoom getLocation return o