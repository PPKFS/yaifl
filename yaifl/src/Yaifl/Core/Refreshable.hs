module Yaifl.Core.Refreshable
  ( refreshRoom
  , refreshThing
  , Refreshable(..)
  ) where

import Yaifl.Prelude


import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Metadata
import Yaifl.Core.ObjectLike
import Yaifl.Core.Store
import Yaifl.Core.Tag
import Data.Bitraversable

-- | All of the objects in the arguments are READ-ONLY. Whilst they can be swapped out, the
-- refresh function is called to replace and update the objects
class Refreshable wm av where
  refresh :: forall es. (NoMissingObjects wm es) => av -> Eff es av

instance {-# OVERLAPPING #-} (HasID o, Refreshable wm o) => Refreshable wm (TaggedObject o tagEntity) where
  refresh obj = tagObject obj <$> refresh (getTaggedObject obj)

instance (Refreshable wm a, Refreshable wm b) => Refreshable wm (a, b) where
  refresh (a, b) = refresh a >>= \a' -> refresh b >>= return . (a', )

instance Refreshable wm a => Refreshable wm (Store a) where
  refresh = mapM refresh

instance Refreshable wm a => Refreshable wm (Maybe a) where
  refresh t = case t of
    Nothing -> return t
    Just x -> Just <$> refresh x

instance (Refreshable wm b, Refreshable wm a) => Refreshable wm (Either a b) where
  refresh = bimapM refresh refresh

instance Refreshable wm Int where
  refresh = pure

instance Refreshable wm (Thing wm) where
  refresh = refreshThing

instance Refreshable wm (AnyObject wm) where
  refresh t = getObject (getID t)

instance Refreshable wm () where
  refresh = const pass

instance Refreshable wm (Room wm) where
  refresh = refreshRoom

refreshRoom ::
  NoMissingObjects wm es
  => RoomLike wm o
  => o
  -> Eff es (Room wm)
refreshRoom tl = do
  r <- getRoom tl
  ifM (traceGuard Medium)
    (do
      r'' <- getRoom (tagRoomEntity r)
      when ((r'' ^. #modifiedTime) /= (r ^. #modifiedTime)) $ noteRuntimeError (const ()) $ "Refreshed room with ID" <> show (display $ view #name r) <> " and found an outdated object"
      return r'')
    (pure r)

refreshThing ::
  NoMissingObjects wm es
  => ThingLike wm o
  => o
  -> Eff es (Thing wm)
refreshThing tl = do
  r <- getThing tl
  ifM (traceGuard Medium)
    (do
      r'' <- getThing (tagThingEntity r)
      when ((r'' ^. #modifiedTime) /= (r ^. #modifiedTime)) $ noteRuntimeError (const ()) $ "Refreshed thing with ID" <> show (display $ view #name r) <> " and found an outdated object"
      return r'')
    (pure r)
