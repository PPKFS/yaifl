{-|
Module      : Yaifl.Properties
Description : Creating, modifying, querying objects.
Copyright   : (c) Avery, 2021
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

module Yaifl.Properties
  ( -- * Adding objects

    addRoom
  , addRoom'
  , addThing
  , addThing'
  , addObject

  -- * querying objects

  , isOpaqueClosedContainer


  -- * get/set/modify

  , move

  , getEnclosing

  , getContainer

  , getEnterable

  , getLocation

  -- * Property stuff
  , HasProperty

  ) where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Messages
import Yaifl.ObjectLookup
import qualified Data.EnumSet as ES
import Yaifl.ObjectLogging
import Yaifl.TH

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

addObject
  :: MonadWorld s m
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
  updWorld (snd obj)
  return (fst obj)

-- | Create a new 'Thing' and add it to the relevant stores.
addThing
  :: MonadWorld s m
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
addThing'
  :: MonadWorld s m
  => Text -- ^ Name.
  -> Text -- ^ Description.
  -> State ThingData r -- ^ Build your own thing monad!
  -> m Entity
addThing' n d stateUpdate = addThing n d (ObjType "thing")
    Nothing (Just $ execState stateUpdate blank) Nothing

-- | Create a new 'Room' and add it to the relevant stores.
addRoom
  :: MonadWorld s m
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
addRoom'
  :: MonadWorld s m
  => Text
  -> Text
  -> State RoomData v
  -> m Entity
addRoom' n d rd = addRoom n d (ObjType "room")
  Nothing (Just (execState rd blank)) Nothing

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

getEnclosing
  :: MonadWorld s m
  => HasProperty s Enclosing
  => ObjectLike s o
  => o
  -> m (Maybe Enclosing)
getEnclosing e = if isThing e
  then
    defaultPropertyGetter e
  else (do
    o <- getRoom e
    return $ o ^? _Just % objData % roomEnclosing
  )

setEnclosing
  :: MonadWorld s m
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

getThingLit
  :: MonadWorld s m
  => ObjectLike s o
  => o
  -> m (Maybe ThingLit)
getThingLit e = if isThing e
  then (do
    o <- getThing e
    return $ o ^? _Just % objData % thingLit)
  else
    return Nothing -- property that makes no sense if it's a room

setThingLit
  :: MonadWorld s m
  => HasID o
  => o
  -> ThingLit
  -> m ()
setThingLit e v = if isThing e
  then
    modifyThing e (objData % thingLit .~ v)
  else
    pass

modifyProperty
  :: MonadWorld s m
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

defaultPropertySetter
  :: MonadWorld s m
  => HasProperty ObjectSpecifics v
  => HasProperty s v
  => HasID o
  => o
  -> v 
  -> m ()
defaultPropertySetter e v = modifyObject e (objSpecifics % propertyL .~ v)

defaultPropertyGetter
  :: MonadWorld s m
  => HasProperty ObjectSpecifics v
  => HasProperty s v
  => ObjectLike s o
  => o
  -> m (Maybe v)
defaultPropertyGetter e = do
  o <- getObject e
  return $ preview (objSpecifics % propertyL) =<< o

makeSpecificsWithout [GetX, SetX] ''Enclosing
makeSpecificsWithout [] ''Container
makeSpecificsWithout [] ''Enterable
--makeSpecificsWithout [GetX, SetX] ''ThingLit --TODO: flag

getLocation
  :: MonadWorld s m
  => ObjectLike s o
  => o
  -> m (Maybe Entity)
getLocation o = do
  v <- getThing o
  let enclosedby = v ^? _Just % containedBy
  v' <- mapMaybeM enclosedby (\x -> if
      isRoom x
    then
      return $ Just x
    else
      getLocation x)
  return $ join v'

move
  :: MonadWorld s m
  => HasProperty s Enclosing
  => Entity
  -> Entity
  -> m Bool
move eObj eLoc = do
  -- reify both objects
  obj <- getThing eObj
  loc <- getEnclosing eLoc
  f <- maybeOrReport2 obj loc
        pass--(logError "Could not find object to move.")
        pass--(logError "Could not find enclosing part of location.")
        (\o' _ -> do
          -- obtain the current container of the thing
          let container = o' ^. containedBy
          oName <- objectName o'
          contName <- objectName container
          eLocName <- objectName eLoc
          --logVerbose $ "Moving " <> oName <> " from " <> contName <> " to " <> eLocName

          -- update the old location
          container `noLongerContains` o'

          -- update the thing moved
          eLoc `nowContains` o'
        )
  return $ isJust f

noLongerContains
  :: MonadWorld s m
  => HasProperty s Enclosing
  => ObjectLike s cont
  => ObjectLike s obj
  => cont
  -> obj
  -> m ()
noLongerContains cont obj = modifyEnclosing cont
  (enclosingContains %~ ES.delete (getID obj))

nowContains
  :: MonadWorld s m
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