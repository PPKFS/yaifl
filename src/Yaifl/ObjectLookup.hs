module Yaifl.ObjectLookup
(
  ObjectLike(..)
  , getThingMaybe
  , asThingOrRoom'
  , getObject
  , setThing
  , setRoom
  , modifyRoom
  , modifyThing
  , modifyObject

  , setAbstractThing
  , setAbstractRoom

  , isType
  , isSupporter

) where

import Yaifl.Prelude
import Yaifl.Common
import Control.Monad.Except

class HasID o => ObjectLike s o where
  getRoom :: (NoMissingObjects s m, MonadWorld s m) => o -> m (Room s)
  default getRoom :: NoMissingObjects s m => o -> m (Room s)
  getRoom o = throwError $ MissingObject "Called getRoom on an object with no instance."  (getID o)
  getThing :: (NoMissingObjects s m, MonadWorld s m) => o -> m (Thing s)
  default getThing :: NoMissingObjects s m => o -> m (Thing s)
  getThing o = throwError $ MissingObject "Called getThing on an object with no instance."  (getID o)

instance ObjectLike s Entity where
  getThing = getObjectFrom things
  getRoom = getObjectFrom rooms

instance ObjectLike s (Thing s) where
  getThing = pure

instance ObjectLike s (Room s) where
  getRoom = pure

instance ObjectLike s (AnyObject s) where
  getThing t = liftEither
    (maybeToRight (MissingObject ("Tried to get a thing from " <> show (_objID t) <> " but it was a room.") (getID t))
      (fromAny t))
  getRoom t = liftEither
    (maybeToRight (MissingObject ("Tried to get a room from " <> show (_objID t) <> " but it was a thing.") (getID t))
      (fromAny t))

instance ObjectLike s (AbstractThing s ) where
  getThing = reifyObject things

instance ObjectLike s (AbstractRoom s ) where
  getRoom = reifyObject rooms

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
  :: NoMissingObjects s m
  => MonadWorld s m
  => HasID o
  => StoreLens' s d
  -> o
  -> m (Object s d)
getObjectFrom l e = do
    o <- getAbstractObjectFrom l e
    reifyObject l o

getAbstractObjectFrom
  :: NoMissingObjects s m
  => MonadReader (World s) m
  => HasID o
  => StoreLens' s d
  -> o
  -> m (AbstractObject s d)
getAbstractObjectFrom l e = do
  w <- ask
  let i = getID e
  liftEither (maybeToRight
    (MissingObject ("Cannot find object with id " <> show i) i) (preview (l % ix i) w) )


getAbstractThing
  :: NoMissingObjects s m
  => MonadReader (World s) m
  => HasID o
  => o
  -> m (AbstractThing s)
getAbstractThing = getAbstractObjectFrom things

getAbstractRoom
  :: NoMissingObjects s m
  => MonadReader (World s) m
  => HasID o
  => o
  -> m (AbstractRoom s)
getAbstractRoom = getAbstractObjectFrom rooms

getObject
  :: NoMissingObjects s m
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> m (AnyObject s)
getObject e = if isThing e
  then
    (do
      o <- getThing e
      return $ toAny o)
  else
    (do
      o <- getRoom e
      return $ toAny o)

getAbstractObject
  :: NoMissingObjects s m
  => MonadWorld s m
  => HasID o
  => o
  -> m (AnyAbstractObject s)
getAbstractObject e = if isThing e
  then
    toAny <$> getAbstractObjectFrom things e
  else
    toAny <$> getAbstractObjectFrom rooms e

getThingMaybe
  :: ObjectLike s o
  => MonadWorld s m
  => o
  -> m (Maybe (Thing s))
getThingMaybe o = withoutMissingObjects (getThing o <&> Just) (const (return Nothing))

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
  :: NoMissingObjects s m
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> (Thing s -> a)
  -> (Room s -> a)
  -> m a
asThingOrRoom' o tf rf =
  if
    isThing o
  then
    tf <$> getThing o
  else
     rf <$> getRoom o

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
  tickGlobalTime False
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
    tickGlobalTime False

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

-- | Calculate whether one object type is a subclass of another
isType
  :: MonadWorldRO s m
  => ObjectLike s o
  => o
  -> ObjType
  -> m Bool
isType _ _ = return False

isSupporter ::
  MonadWorldRO s m
  => ObjectLike s o
  => o
  -> m Bool
isSupporter = (`isType` ObjType "supporter")