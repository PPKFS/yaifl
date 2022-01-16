module Yaifl.ObjectLookup
(
  ObjectLike(..)
  , asThingOrRoom'
  , getObject
  , setThing
  , setRoom
  , modifyRoom
  , modifyThing
  , modifyObject

  , setAbstractThing
  , setAbstractRoom

) where

import Yaifl.Prelude
import Yaifl.Common
class HasID o => ObjectLike s o where
  getRoom :: MonadWorld s m => o -> m (Maybe (Room s))
  default getRoom :: MonadWorld s m => o -> m (Maybe (Room s))
  --todo: add logging error here
  getRoom = const $ return Nothing
  getThing :: MonadWorld s m => o -> m (Maybe (Thing s))
  default getThing :: MonadWorld s m => o -> m (Maybe (Thing s))
  --todo: add logging error here
  getThing = const $ return Nothing

instance ObjectLike s Entity where
  getThing = getObjectFrom things
  getRoom = getObjectFrom rooms

instance ObjectLike s (Thing s) where
  getThing = return . Just

instance ObjectLike s (Room s) where
  getRoom = return . Just

instance ObjectLike s (AnyObject s) where
  getThing = return . fromAny
  getRoom = return . fromAny

-- ** getX
-- sets of signatures
-- internal: getObjectFrom, getAbstractObjectFrom
-- if we know what we are getting and we want reification
-- getThing, getRoom
-- if we know what we are getting and we don't want reification:
-- getAbstractThing, getAbstractRoom
-- if we don't know what we are getting and we want reification
-- getObject
-- if we don't know what we are getting and we don't want reification
-- getAbstractObject
-- affine traversals
-- object, abstractObject, thing, room
getObjectFrom
  :: MonadWorld s m
  => HasID o
  => StoreLens' s d
  -> o
  -> m (Maybe (Object s d))
getObjectFrom l e = do
    o <- gets $ getAbstractObjectFrom l e
    mapMaybeM o (reifyObject l)

getAbstractObjectFrom
  :: MonadReader (World s) m
  => HasID o
  => StoreLens' s d
  -> o
  -> m (Maybe (AbstractObject s d))
getAbstractObjectFrom l e = do
  w <- ask
  return $ w & preview (l % ix (getID e))

getAbstractThing
  :: MonadReader (World s) m
  => HasID o
  => o
  -> m (Maybe (AbstractThing s))
getAbstractThing = getAbstractObjectFrom things

getAbstractRoom
  :: MonadReader (World s) m
  => HasID o
  => o
  -> m (Maybe (AbstractRoom s))
getAbstractRoom = getAbstractObjectFrom rooms

getObject
  :: MonadWorld s m
  => ObjectLike s o
  => o
  -> m (Maybe (AnyObject s))
getObject e = if isThing e
  then
    (do
      o <- getThing e
      return $ toAny <$> o)
  else
    (do
      o <- getRoom e
      return $ toAny <$> o)

getAbstractObject
  :: HasID o
  => o
  -> World s
  -> Maybe (AnyAbstractObject s)
getAbstractObject e w = if isThing e
  then
    toAny <$> getAbstractObjectFrom things e w
  else
    toAny <$> getAbstractObjectFrom rooms e w

{-
-- the getter on this doesn't update the cache...
-- but it does return an updated object.
object
  :: ObjectLike s o
  => o
  -> AffineTraversal' (World s) (AnyObject s)
object e = atraversal
  (if isThing e
    then
      \w -> maybeToRight w $ toAny <$> evalState (getThing e) w
    else
      \w -> maybeToRight w $ toAny <$> evalState (getRoom e) w)
  (\w o -> execState (setObject o) w)
-}
setObject
  :: MonadWorld s m
  => AnyObject s
  -> m ()
setObject o = modifyObject o id

modifyObject
  :: MonadWorld s m
  => HasID o
  => o
  -> (AnyObject s -> AnyObject s)
  -> m ()
modifyObject e s = if isThing e
  then
    modifyObjectFrom things e (anyModifyToThing s)
  else
    modifyObjectFrom rooms e (anyModifyToRoom s)

anyModifyToThing
  :: (AnyObject s -> AnyObject s)
  -> (Thing s -> Thing s)
anyModifyToThing f t = fromMaybe t (fromAny $ f (toAny t))

anyModifyToRoom
  :: (AnyObject s -> AnyObject s)
  -> (Room s -> Room s)
anyModifyToRoom f t = fromMaybe t (fromAny $ f (toAny t))

asThingOrRoom'
  :: MonadWorld s m
  => ObjectLike s o
  => o
  -> (Thing s -> a)
  -> (Room s -> a)
  -> m (Maybe a)
asThingOrRoom' o tf rf =
  if
    isThing o
  then
    tf <$$> getThing o
  else
     rf <$$> getRoom o

modifyObjectFrom
  :: MonadWorld s m
  => HasID o
  => StoreLens' s d
  -> o
  -> (Object s d -> Object s d)
  -> m ()
modifyObjectFrom l o s = do
  ts <- gets getGlobalTime
  l % ix (getID o) % objectL ts %= s
  tickGlobalTime
  pass

setObjectFrom
  :: MonadWorld s m
  => StoreLens' s d
  -> Object s d
  -> m ()
setObjectFrom l o = modifyObjectFrom l o id

modifyThing
  :: MonadWorld s m
  => HasID o
  => o
  -> (Thing s -> Thing s)
  -> m ()
modifyThing = modifyObjectFrom things

modifyRoom
  :: MonadWorld s m
  => HasID o
  => o
  -> (Room s -> Room s)
  -> m ()
modifyRoom = modifyObjectFrom rooms

setThing
  :: MonadWorld s m
  => Thing s
  -> m ()
setThing = setObjectFrom things

setRoom
  :: MonadWorld s m
  => Room s
  -> m ()
setRoom = setObjectFrom rooms

setAbstractObjectFrom
  :: MonadWorld s m
  => StoreLens' s d
  -> AbstractObject s d
  -> m ()
setAbstractObjectFrom l o = do
    l % at (getID o) ?= o
    tickGlobalTime

setAbstractThing
  :: MonadWorld s m
  => AbstractThing s
  -> m ()
setAbstractThing = setAbstractObjectFrom things

setAbstractRoom
  :: MonadWorld s m
  => AbstractRoom s
  -> m ()
setAbstractRoom = setAbstractObjectFrom rooms