module Yaifl.ObjectLookup
(
  ObjectLike(..)
  , asThingOrRoom'
  , getObject
  , getThing'
  , getObject'
  , setThing
  , setRoom
  , setThing'
  , setRoom'
  , setAbstractThing'
  , setAbstractRoom'
  , modifyRoom
  , modifyThing
  , modifyObject

) where

import Yaifl.Prelude
import Yaifl.Common
class HasID o => ObjectLike s o where
  getRoom :: o -> State (World s) (Maybe (Room s))
  default getRoom :: o -> State (World s) (Maybe (Room s))
  getRoom = const $ return Nothing
  getThing :: o -> State (World s) (Maybe (Thing s))
  default getThing :: o -> State (World s) (Maybe (Thing s))
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
  :: HasID o
  => StoreLens' s d
  -> o
  -> State (World s) (Maybe (Object s d))
getObjectFrom l e = do
    o <- gets $ getAbstractObjectFrom l e
    mapMaybeM o (state . reifyObject l)

getAbstractObjectFrom
  :: HasID o
  => StoreLens' s d
  -> o
  -> World s
  -> Maybe (AbstractObject s d)
getAbstractObjectFrom l e w = w ^? l % ix (getID e)

getAbstractThing
  :: HasID o
  => o
  -> World s
  -> Maybe (AbstractThing s)
getAbstractThing = getAbstractObjectFrom things

getAbstractRoom
  :: HasID o
  => o
  -> World s
  -> Maybe (AbstractRoom s)
getAbstractRoom = getAbstractObjectFrom rooms

getThing'
  :: ObjectLike s o
  => o
  -> World s
  -> Maybe (Thing s)
getThing' o = evalState (getThing o)

getRoom'
  :: ObjectLike s o
  => o
  -> World s
  -> Maybe (Room s)
getRoom' o = evalState (getRoom o)

getObject
  :: ObjectLike s o
  => o
  -> State (World s) (Maybe (AnyObject s))
getObject e = if isThing e
  then
    (do
      o <- getThing e
      return $ toAny <$> o)
  else
    (do
      o <- getRoom e
      return $ toAny <$> o)

getObject'
  :: ObjectLike s o
  => o
  -> World s
  -> Maybe (AnyObject s)
getObject' o = evalState (getObject o)

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

setObject
  :: AnyObject s
  -> State (World s) ()
setObject o = modifyObject o id

modifyObject
  :: HasID o
  => o
  -> (AnyObject s -> AnyObject s)
  -> State (World s) ()
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
  :: ObjectLike s o
  => o
  -> (Thing s -> a)
  -> (Room s -> a)
  -> World s
  -> Maybe a
asThingOrRoom' o tf rf w =
  if
    isThing o
  then
      tf <$> getThing' o w
  else
     rf <$> getRoom' o w

modifyObjectFrom
  :: HasID o
  => StoreLens' s d
  -> o
  -> (Object s d -> Object s d)
  -> State (World s) ()
modifyObjectFrom l o s = do
  l % ix (getID o) % objectL %= s
  tickGlobalTime
  pass

setObjectFrom
  :: StoreLens' s d
  -> Object s d
  -> State (World s) ()
setObjectFrom l o = modifyObjectFrom l o id

modifyThing
  :: HasID o
  => o
  -> (Thing s -> Thing s)
  -> State (World s) ()
modifyThing = modifyObjectFrom things

modifyRoom
  :: HasID o
  => o
  -> (Room s -> Room s)
  -> State (World s) ()
modifyRoom = modifyObjectFrom rooms

setThing
  :: Thing s
  -> State (World s) ()
setThing = setObjectFrom things

setRoom
  :: Room s
  -> State (World s) ()
setRoom = setObjectFrom rooms

setThing'
  :: Thing s
  -> World s
  -> World s
setThing' = execState . setThing

setRoom'
  :: Room s
  -> World s
  -> World s
setRoom' = execState . setRoom

setAbstractObjectFrom
  :: StoreLens' s d
  -> AbstractObject s d
  -> State (World s) ()
setAbstractObjectFrom l o = do
    l % at (getID o) ?= o
    tickGlobalTime

setAbstractThing
  :: AbstractThing s
  -> State (World s) ()
setAbstractThing = setAbstractObjectFrom things

setAbstractRoom
  :: AbstractRoom s
  -> State (World s) ()
setAbstractRoom = setAbstractObjectFrom rooms

setAbstractThing'
  :: AbstractThing s
  -> World s
  -> World s
setAbstractThing' = execState . setAbstractThing

setAbstractRoom'
  :: AbstractRoom s
  -> World s
  -> World s
setAbstractRoom' = execState . setAbstractRoom