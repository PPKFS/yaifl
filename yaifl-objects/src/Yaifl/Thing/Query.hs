module Yaifl.Thing.Query
  ( getLocation
  , forThing
  ) where

import Yaifl.Prelude



import Yaifl.Effects.ObjectQuery
import Yaifl.AnyObject
import Yaifl.Object.Kind
import Yaifl.Room.Kind
import Yaifl.ObjectLike
import Yaifl.Thing.Kind
import Yaifl.Rulebook
import Yaifl.Object.Query

forThing ::
  WithoutMissingObjects wm es
  => ObjectLike wm a
  => a
  -> (Thing wm -> Eff es (Maybe b, Maybe r))
  -> Eff es (Maybe b, Maybe r)
forThing e = ruleWhenJustM (getThingMaybe e)

getLocation ::
  WithoutMissingObjects wm es
  => ThingLike wm o
  => o
  -> Eff es (Room wm)
getLocation t = do
  t' <- getThing t
  o <- getObject (t' ^. #objectData % #containedBy)
  asThingOrRoom getLocation return o