{-|
Module      : Yaifl.Properties
Description : Creating, modifying, querying objects.
Copyright   : (c) Avery, 2021
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Properties
  ( -- * Specifics
  ObjectSpecifics(..)
{-
    addRoom
  , addRoom'
  , addThing
  , addThing'
  , addObject
  , addBaseObjects

  -- * querying objects

  , isOpaqueClosedContainer
  , withoutMissingObjects
  , handleMissingObject


  -- * get/set/modify

  , move

  , getEnclosing

  , getContainer

  , getEnterable

  , getLocation

  , getOpenable

  , getThingLit
  , getWornBy

  -- * Property stuff
  , HasProperty
-}
  ) where

import Solitude
import Yaifl.Common
import Yaifl.Properties.Enclosing
import Yaifl.Properties.Container
import Yaifl.Properties.Openable

data ObjectSpecifics =
  NoSpecifics
  | EnclosingSpecifics Enclosing
  | ContainerSpecifics Container 
  | OpenableSpecifics Openable
  deriving stock (Show)

{--
-- | Create a new object and assign it an entity ID, but do **not** add it to any
-- stores. See also 'addObject' for a version that adds it to a store.
makeObject
  :: MonadWorld s m
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType
  -> Bool
  -> Either ObjectSpecifics s -- ^ Object details.
  -> d
  -> Maybe (ObjectUpdate s d) -- ^ 'Nothing' for a static object, 'Just f' for
                                  -- a dynamic object.
  -> m (Entity, AbstractObject s d)
makeObject n d ty isT specifics details upd = do
  e <- state $ newEntityID isT
  t <- gets getGlobalTime
  let obj = Object n d e ty t specifics details
  return (e, maybe (StaticObject obj) (DynamicObject . TimestampedObject obj t) upd)


class HasProperty o v where
  default propertyL :: AffineTraversal' o v
  propertyL = atraversal Left const
  propertyL :: AffineTraversal' o v

instance (HasProperty a v, HasProperty b v) => HasProperty (Either a b) v where
  propertyL = propertyL `eitherJoin` propertyL

instance HasProperty ObjectSpecifics Enclosing where
  propertyL = _EnclosingSpecifics `thenATraverse` (_ContainerSpecifics % containerEnclosing)
instance HasProperty () a

instance HasProperty ObjectSpecifics Container where
  propertyL = castOptic _ContainerSpecifics

instance HasProperty ObjectSpecifics Enterable where
  propertyL = _ContainerSpecifics % containerEnterable

instance HasProperty ObjectSpecifics Openable where
  propertyL = _OpenableSpecifics `thenATraverse` (_ContainerSpecifics % containerOpenable)

getEnclosing
  :: NoMissingObjects s m
  => MonadWorldNoLog s m
  => HasProperty s Enclosing
  => ObjectLike s o
  => o
  -> m (Maybe Enclosing)
getEnclosing e = if isThing e
  then
    defaultPropertyGetter e
  else (do
    o <- getRoom e
    return $ Just $ o ^. objData % roomEnclosing
  )

setEnclosing
  :: MonadWorldNoLog s m
  => HasProperty s Enclosing
  => HasID o
  => o
  -> Enclosing
  -> m ()
setEnclosing e v = if isThing e
  then
    defaultPropertySetter e v
  else
    modifyRoom e (objData % roomEnclosing .~ v)

getThingLit :: 
  NoMissingObjects s m
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> m (Maybe ThingLit)
getThingLit e = if isThing e
  then (do
    o <- getThing e
    return $ Just $ o ^. objData % thingLit)
  else
    return Nothing -- property that makes no sense if it's a room

setThingLit :: 
  MonadWorld s m
  => HasID o
  => o
  -> ThingLit
  -> m ()
setThingLit e v = if isThing e
  then
    modifyThing e (objData % thingLit .~ v)
  else
    pass

getWornBy :: 
  NoMissingObjects s m
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> m (Maybe Entity)
getWornBy e = if isThing e
  then (do
    o <- getThing e
    return $ o ^? objData % thingWearable % _Wearable % _Just)
  else
    return Nothing -- property that makes no sense if it's a room

setWornBy :: 
  MonadWorld s m
  => HasID o
  => o
  -> Maybe Entity
  -> m ()
setWornBy e v = if isThing e
  then
    modifyThing e (objData % thingWearable % _Wearable .~ v)
  else
    pass

modifyProperty :: 
  MonadWorldNoLog s m
  => (o -> m (Maybe p))
  -> (o -> p -> m ())
  -> o
  -> (p -> p)
  -> m ()
modifyProperty g s o f = do
  e <- g o
  when (isNothing e) (do
    --logVerbose "Trying to modify a property of an object which does not exist"
    pass)
  whenJust e (s o . f)

defaultPropertySetter :: 
  MonadWorldNoLog s m
  => HasProperty ObjectSpecifics v
  => HasProperty s v
  => HasID o
  => o
  -> v 
  -> m ()
defaultPropertySetter e v = modifyObject e (objSpecifics % propertyL .~ v)

defaultPropertyGetter
  :: NoMissingObjects s m
  => MonadWorldNoLog s m
  => HasProperty ObjectSpecifics v
  => HasProperty s v
  => ObjectLike s o
  => o
  -> m (Maybe v)
defaultPropertyGetter e = do
  o <- getObject e
  return $ preview (objSpecifics % propertyL) o

makeSpecificsWithout [GetX, SetX] ''Enclosing
makeSpecificsWithout [] ''Container
makeSpecificsWithout [] ''Enterable
makeSpecificsWithout [] ''Openable
--makeSpecificsWithout [GetX, SetX] ''ThingLit --TODO: flag

getLocation
  :: NoMissingObjects s m
  => MonadWorld s m
  => ObjectLike s o
  => o
  -> m Entity
getLocation o = do
  v <- getThing o
  let enclosedby = v ^. containedBy
  if
    isRoom enclosedby
  then
    return enclosedby
  else
    getLocation enclosedby

move :: 
  HasCallStack
  => MonadWorld s m
  => HasProperty s Enclosing
  => ObjectLike s o1
  => ObjectLike s o2
  => o1
  -> o2
  -> m Bool
move eObj eLoc = withoutMissingObjects (do
  -- reify both objects
  o' <- getThing eObj
  loc <- isJust <$> getEnclosing eLoc
  debug $ TLB.fromText (prettify o')
  if 
    loc
  then
    isJust <$> (do
      -- obtain the current container of the thing
      let container = o' ^. containedBy
      oName <- objectName o'
      contName <- objectName container
      eLocName <- objectName eLoc
      debug $ bformat ("Moving " %! stext %! " from " %! stext %! " to " %! stext) oName contName eLocName
      -- update the old location
      container `noLongerContains` o'
      -- update the thing moved
      eLoc `nowContains` o'
      tickGlobalTime True
      return $ Just True)
  else
    throwError $ MissingObject "Could not find enclosing part of location." (getID eLoc))
    (handleMissingObject (bformat ("Failed to move ObjectID " %! int %! " to ObjectID " %! int ) (getID eObj) (getID eLoc)) False)



handleMissingObject :: (HasCallStack, Logger m) => TLB.Builder -> a ->  MissingObject s -> m a
handleMissingObject msg def (MissingObject t o) = do
  err (msg <> bformat (stext %! "; Object ID: " %! stext) t (show o))
  return def

noLongerContains
  :: NoMissingObjects s m
  => MonadWorld s m
  => HasProperty s Enclosing
  => ObjectLike s cont
  => ObjectLike s obj
  => cont
  -> obj
  -> m ()
noLongerContains cont obj = modifyEnclosing cont
  (enclosingContains %~ ES.delete (getID obj))

nowContains
  :: NoMissingObjects s m
  => MonadWorld s m
  => HasProperty s Enclosing
  => ObjectLike s cont
  => ObjectLike s obj
  => cont
  -> obj
  -> m ()
nowContains cont obj = do
  modifyEnclosing cont (enclosingContains %~ ES.insert (getID obj))
  modifyThing obj (objData % thingContainedBy .~ getID cont)

isOpaqueClosedContainer
  :: Container
  -> Bool
isOpaqueClosedContainer c = (_containerOpacity c == Opaque) && (_containerOpenable c == Closed)


addObject :: 
  MonadWorld s m
  => HasProperty s Enclosing
  => (ObjectLike s (AbstractObject s d))
  => (forall m1. MonadWorld s m1 => AbstractObject s d -> m1 ())
  -> Text
  -> Text
  -> ObjType
  -> Bool
  -> Either ObjectSpecifics s
  -> d
  -> Maybe (ObjectUpdate s d)
  -> m Entity
-- hilariously the pointfree version of this is
-- addObject = (. makeObject) . (.) . (.) . (.) . (.) . (.) . uncurry
addObject updWorld n d ty isT specifics details updateFunc = do
  obj <- makeObject n d ty isT specifics details updateFunc
  debug $ bformat ("Made a new " %! stext %! " called " %! stext %! " with ID " %! int)
    (if isThing (snd obj) then "thing" else "room") n (getID $ snd obj)
  updWorld (snd obj)
  let e = snd obj
  asThing <- getThingMaybe e
  lastRoom <- use previousRoom
  case asThing of
    Nothing -> previousRoom .= fst obj
    Just t -> move t lastRoom >> pass -- move it if we're still 
    
  return (fst obj)

-- | Create a new 'Thing' and add it to the relevant stores.
addThing ::
  MonadWorld s m
  => HasProperty s Enclosing
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (Either ObjectSpecifics s)
  -> Maybe ThingData -- ^ Optional details; if 'Nothing' then the default is used.
  -> Maybe (ObjectUpdate s ThingData) -- ^ Static/Dynamic.
  -> m Entity
addThing name desc objtype specifics details = addObject setAbstractThing name desc objtype
  True (fromMaybe (Left NoSpecifics) specifics) (fromMaybe blank details)

-- | A version of 'addThing' that uses a state monad to provide imperative-like
-- descriptions of the internals of the object. Compare
-- @
-- addThing n d o (Just $ (ThingData default default default .. mod1)) ...
-- @ with @
-- addThing' n d o (someLensField .= 5)
-- @
addThing' :: 
  MonadWorld s m
  => HasProperty s Enclosing
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> State ThingData r -- ^ Build your own thing monad!
  -> m Entity
addThing' n d stateUpdate = addThing n d (ObjType "thing")
    Nothing (Just $ execState stateUpdate blank) Nothing

-- | Create a new 'Room' and add it to the relevant stores.
addRoom
  :: MonadWorld s m
  => HasProperty s Enclosing
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (Either ObjectSpecifics s)
  -> Maybe RoomData -- ^
  -> Maybe (ObjectUpdate s RoomData)  -- ^
  -> m Entity
addRoom name desc objtype specifics details upd = do
  e <- addObject setAbstractRoom name desc objtype False
        (fromMaybe (Left NoSpecifics) specifics) (fromMaybe blank details) upd
  w <- get
  when (isNothing $ w ^. firstRoom) (firstRoom ?= e)
  return e

-- | A version of 'addRoom' that uses a state monad to provide imperative-like
-- descriptions of the internals of the object. Compare
-- @
-- addThing n d o (Just $ (ThingData default default default .. mod1)) ...
-- @ with @
-- addThing' n d o (someLensField .= 5)
-- @
addRoom' :: 
  MonadWorld s m
  => HasProperty s Enclosing
  => Text
  -> Text
  -> State RoomData v
  -> m Entity
addRoom' n d rd = addRoom n d (ObjType "room")
  Nothing (Just (execState rd blank)) Nothing

addBaseObjects ::
  MonadWorld s m
  => HasProperty s Enclosing
  => m ()
addBaseObjects = do
  addRoom' "The Void" "If you're seeing this, you did something wrong." pass
  addThing' "player" "It's you, looking handsome as always" (
    thingDescribed .= Undescribed)
  firstRoom .= Nothing
-}