{-|
Module      : Yaifl.Objects.Query
Description : Getting (and modifying) objects from the world.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

-- for ObjectLike wm Entity
{-# OPTIONS_GHC -Wno-orphans #-}

module Yaifl.Objects.Query
  ( -- * Get
    getAbstractThing
  , getAbstractRoom
  , getThing
  , getRoom
  , getObject
  , getAbstractObject
  , getThingMaybe
  , getRoomMaybe
  , getObjectFrom

    -- * Set
  , setObject
  , setThing
  , setRoom
  , setAbstractThing
  , setAbstractRoom
  , setAbstractObjectFrom
  , setObjectFrom
    -- * Modify
  , modifyObject
  , modifyThing
  , modifyRoom
  ) where
    
import Yaifl.Common
import Yaifl.Objects.Dynamic
import Yaifl.WorldInfo
import Yaifl.Objects.Missing
import Yaifl.Objects.Object
import Solitude
import Control.Monad.Except (liftEither)
  
-- * Get from a specific store

instance ObjectLike wm Entity where
  getThing = getObjectFrom things
  getRoom = getObjectFrom rooms

getAbstractObjectFrom :: 
  NoMissingObjects m
  => MonadReader (World wm) m
  => HasID o
  => StoreLens' wm d
  -> o
  -> m (AbstractObject wm d)
getAbstractObjectFrom l e = do
  w <- ask
  let i = getID e
  liftEither (maybeToRight
    (MissingObject ("Cannot find object with id " <> show i) i) (preview (l % ix i) w) )
    
getObjectFrom ::
  NoMissingObjects m
  => MonadWorld wm m
  => HasID o
  => StoreLens' wm d
  -> o
  -> m (Object wm d)
getObjectFrom l e = getAbstractObjectFrom l e >>= reifyObject l

-- * Get abstract

getAbstractThing :: 
  NoMissingObjects m
  => MonadReader (World wm) m
  => HasID o
  => o
  -> m (AbstractThing wm)
getAbstractThing = getAbstractObjectFrom things

getAbstractRoom ::
  NoMissingObjects m
  => MonadReader (World wm) m
  => HasID o
  => o
  -> m (AbstractRoom wm)
getAbstractRoom = getAbstractObjectFrom rooms

-- * Get any

getObject ::
  NoMissingObjects m
  => MonadWorld wm m
  => ObjectLike wm o
  => o
  -> m (AnyObject wm)
getObject e = if isThing e
  then (do
      o <- getThing e
      return $ review _Thing o)
  else (do
      o <- getRoom e
      return $ review _Room o)

getAbstractObject :: 
  NoMissingObjects m
  => MonadWorld wm m
  => HasID o
  => o
  -> m (AnyAbstractObject wm)
getAbstractObject e = 
  if isThing e
  then review _AbstractThing <$> getAbstractObjectFrom things e
  else review _AbstractRoom <$> getAbstractObjectFrom rooms e

-- * without a `NoMissingObjects` clause
getThingMaybe :: 
  ObjectLike wm o
  => MonadWorld wm m
  => o
  -> m (Maybe (Thing wm))
getThingMaybe o = withoutMissingObjects (getThing o <&> Just) (const (return Nothing))

getRoomMaybe ::
  ObjectLike wm o
  => MonadWorld wm m
  => o
  -> m (Maybe (Room wm))
getRoomMaybe o = withoutMissingObjects (getRoom o <&> Just) (const (return Nothing))

-- * Setting and modifying

modifyObjectFrom ::
  MonadWorld wm m
  => HasID o
  => StoreLens' wm d
  -> o
  -> (Object wm d -> Object wm d)
  -> m ()
modifyObjectFrom l o s = do
  ts <- gets getGlobalTime
  l % ix (getID o) % objectL ts %= s
  tickGlobalTime False
  pass

setObjectFrom :: 
  MonadWorld wm m
  => StoreLens' wm d
  -> Object wm d
  -> m ()
setObjectFrom l o = modifyObjectFrom l o id

setObject
  :: MonadWorld wm m
  => AnyObject wm
  -> m ()
setObject o = modifyObject o id

modifyObject ::
  MonadWorld wm m
  => HasID o
  => o
  -> (AnyObject wm -> AnyObject wm)
  -> m ()
modifyObject e s = 
  if isThing e
  then modifyObjectFrom things e (anyModifyToThing s)
  else modifyObjectFrom rooms e (anyModifyToRoom s)

anyModifyToThing :: 
  (AnyObject s -> AnyObject s)
  -> (Thing s -> Thing s)
anyModifyToThing f t = fromMaybe t (preview _Thing $ f (review _Thing t))

anyModifyToRoom :: 
  (AnyObject s -> AnyObject s)
  -> (Room s -> Room s)
anyModifyToRoom f t = fromMaybe t (preview _Room $ f (review _Room t))

-- * Setting and modifying rooms/things

modifyThing :: 
  MonadWorld wm m
  => HasID o
  => o
  -> (Thing wm -> Thing wm)
  -> m ()
modifyThing = modifyObjectFrom things

modifyRoom ::
  MonadWorld wm m
  => HasID o
  => o
  -> (Room wm -> Room wm)
  -> m ()
modifyRoom = modifyObjectFrom rooms

setThing :: 
  MonadWorld wm m
  => Thing wm
  -> m ()
setThing = setObjectFrom things

setRoom ::
  MonadWorld wm m
  => Room wm
  -> m ()
setRoom = setObjectFrom rooms

setAbstractObjectFrom :: 
  MonadWorld wm m
  => StoreLens' wm d
  -> AbstractObject wm d
  -> m ()
setAbstractObjectFrom l o = do
    l % at (getID o) ?= o
    tickGlobalTime False

setAbstractThing :: 
  MonadWorld wm m
  => AbstractThing wm
  -> m ()
setAbstractThing = setAbstractObjectFrom things

setAbstractRoom :: 
  MonadWorld wm m
  => AbstractRoom wm
  -> m ()
setAbstractRoom = setAbstractObjectFrom rooms

asThingOrRoom :: 
  NoMissingObjects m
  => MonadWorld wm m
  => ObjectLike wm o
  => o
  -> (Thing wm -> a)
  -> (Room wm -> a)
  -> m a
asThingOrRoom o tf rf =
  if isThing o
  then tf <$> getThing o
  else rf <$> getRoom o