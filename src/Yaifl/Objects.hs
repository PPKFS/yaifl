{-|
Module      : Yaifl.Objects
Description : Creating, modifying, querying objects.
Copyright   : (c) Avery, 2021
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# OPTIONS_GHC -ddump-splices #-}
module Yaifl.Objects
  ( -- * Adding objects

    addRoom
  , addRoom'
  , addThing
  , addThing'
  , addObject

  -- * querying objects

  , isOpaqueClosedContainer
  , ObjectLike(..)
  , asThingOrRoom'

  -- * get/set/modify
  , containedBy

  , move

  , getObject
  , getThing'
  , getObject'

  , getEnclosing
  , getEnclosing'

  , getContainer'
  , getContainer

  , getEnterable
  , getEnterable'


  , fromAny
  , toAny

  , getLocation
  , getLocation'

  -- * Property stuff
  , HasProperty

  , Prettify(..)
  , shortPrint

  ) where

import Yaifl.Prelude
import Yaifl.Common
import Yaifl.Messages
import qualified Data.EnumSet as ES
import Text.Pretty.Simple
import Yaifl.TH

containedBy :: Lens' (Thing s) Entity
containedBy = objData % thingContainedBy

class Prettify o where
  prettify :: o -> Text


instance {-# OVERLAPPABLE #-} Prettify s where
  prettify = const "No prettify instance"

instance Prettify o => Prettify (Maybe o) where
  prettify Nothing = "Nothing"
  prettify (Just s) = prettify s

instance Prettify Text where
  prettify = id
instance Prettify (Object s d) where
  prettify Object{..} = _objName <> " (ID: " <>  show (unID _objID) <> ")\n" <> toStrict (pString (toString s)) where
    s = "{ Description = " <> _objDescription <>
        ", Type = " <> prettify _objType <>
        -- F.% ", Creation Time = " F.% F.stext
        ", Specifics = " <> prettify _objSpecifics <>
        ", Data = " <> prettify _objData

instance Prettify ObjType where
  prettify = unObjType

instance Prettify (Either a b) where
  prettify = either prettify prettify

instance Prettify [a] where
  prettify e = mconcat $ map prettify e

shortPrint
  :: Object s d
  -> Text
shortPrint Object{..} = _objName <> " (ID: " <>  show (unID _objID) <> ")"
logObject
  :: ObjectLike s o
  => Text
  -> o
  -> State (World s) ()
logObject n e = do
  o <- getObject e
  logVerbose $ n <> "\n" <> prettify o
  whenJust o $ \Object{..} -> logVerbose _objName

objectName
  :: ObjectLike s o
  => o
  -> World s
  -> Text
objectName o w = maybe "Nothin" _objName $ evalState (getObject o) w

-- | Create a new object and assign it an entity ID, but do **not** add it to any
-- stores. See also 'addObject' for a version that adds it to a store.
makeObject
  :: Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType
  -> Bool
  -> Either ObjectSpecifics s -- ^ Object details.
  -> d
  -> Maybe (ObjectUpdate s d) -- ^ 'Nothing' for a static object, 'Just f' for
                                  -- a dynamic object.
  -> World s
  -> ((Entity, AbstractObject s d), World s)
makeObject n d ty isT specifics details upd = runState $ do
  e <- state $ newEntityID isT
  t <- gets getGlobalTime
  let obj = Object n d e ty t specifics details
  return (e, maybe (StaticObject obj) (DynamicObject . TimestampedObject obj t) upd)

addObject
  :: (AbstractObject s d -> World s -> World s)
  -> Text
  -> Text
  -> ObjType
  -> Bool
  -> Either ObjectSpecifics s
  -> d
  -> Maybe (ObjectUpdate s d)
  -> State (World s) Entity
-- hilariously the pointfree version of this is
-- addObject = (. makeObject) . (.) . (.) . (.) . (.) . (.) . uncurry
addObject updWorld n d ty isT specifics details updateFunc = do
  obj <- state $ makeObject n d ty isT specifics details updateFunc
  modify $ updWorld (snd obj)
  return (fst obj)

updateInternal
  :: Lens' (World s) (Store (AbstractObject s d))
  -> AbstractObject s d
  -> World s
  -> World s
updateInternal storeLens obj = storeLens % at (getID obj) ?~ obj

-- | Update a 'thing'.
updateThing
  :: AbstractThing s
  -> World s
  -> World s
updateThing = updateInternal things

-- | Update a 'room'.
updateRoom
  :: AbstractRoom s
  -> World s
  -> World s
updateRoom = updateInternal rooms

-- | Create a new 'Thing' and add it to the relevant stores.
addThing
  :: Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (Either ObjectSpecifics s)
  -> Maybe ThingData -- ^ Optional details; if 'Nothing' then the default is used.
  -> Maybe (ObjectUpdate s ThingData) -- ^ Static/Dynamic.
  -> State (World s) Entity
addThing name desc objtype specifics details = addObject updateThing name desc objtype
  True (fromMaybe (Left NoSpecifics) specifics) (fromMaybe blankThingData details)

-- | A version of 'addThing' that uses a state monad to provide imperative-like
-- descriptions of the internals of the object. Compare
-- @
-- addThing n d o (Just $ (ThingData default default default .. mod1)) ...
-- @ with @
-- addThing' n d o (someLensField .= 5)
-- @
addThing'
  :: Text -- ^ Name.
  -> Text -- ^ Description.
  -> State ThingData r -- ^ Build your own thing monad!
  -> State (World s) Entity
addThing' n d stateUpdate = addThing n d (ObjType "thing")
    Nothing (Just $ execState stateUpdate blankThingData) Nothing

-- | Create a new 'Room' and add it to the relevant stores.
addRoom
  :: Text -- ^ Name.
  -> Text -- ^ Description.
  -> ObjType -- ^ Type.
  -> Maybe (Either ObjectSpecifics s)
  -> Maybe RoomData -- ^
  -> Maybe (ObjectUpdate s RoomData)  -- ^
  -> State (World s) Entity
addRoom name desc objtype specifics details upd = do
  e <- addObject updateRoom name desc objtype False
        (fromMaybe (Left NoSpecifics) specifics) (fromMaybe blankRoomData details) upd
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
  :: Text
  -> Text
  -> State RoomData v
  -> State (World s) Entity
addRoom' n d rd = addRoom n d (ObjType "room")
  Nothing (Just (execState rd blankRoomData)) Nothing

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

class CanBeAny o d where
  toAny :: o -> d
  fromAny :: d -> Maybe o

instance CanBeAny (Object s RoomData) (AnyObject s) where
  toAny = fmap Right
  fromAny = traverse rightToMaybe

instance CanBeAny (Object s ThingData) (AnyObject s) where
  toAny = fmap Left
  fromAny = traverse leftToMaybe

instance CanBeAny (AbstractObject s RoomData) (AnyAbstractObject s) where
  toAny (StaticObject s) = StaticObject $ toAny s
  toAny (DynamicObject (TimestampedObject tsobj tsts tsf)) =
    DynamicObject $ TimestampedObject
    (toAny tsobj) tsts (\a w -> maybe a (\v -> Right <$> tsf v w) (fromAny a))

  fromAny (StaticObject s) = fmap StaticObject (fromAny s)
  fromAny (DynamicObject (TimestampedObject tsobj tsts tsf)) = case fromAny tsobj of
    Nothing -> Nothing
    Just s -> Just $ DynamicObject
      (TimestampedObject s tsts (\v w -> fromMaybe v (fromAny $ tsf (toAny v) w) ))

instance CanBeAny (AbstractObject s ThingData) (AnyAbstractObject s) where
  toAny (StaticObject s) = StaticObject $ toAny s
  toAny (DynamicObject (TimestampedObject tsobj tsts tsf)) =
    DynamicObject $ TimestampedObject
    (toAny tsobj) tsts (\a w -> maybe a (\v -> Left <$> tsf v w) (fromAny a))
  fromAny (StaticObject s) = fmap StaticObject (fromAny s)
  fromAny (DynamicObject (TimestampedObject tsobj tsts tsf)) = case fromAny tsobj of
    Nothing -> Nothing
    Just s -> Just $ DynamicObject
      (TimestampedObject s tsts
        (\v w -> fromMaybe v (fromAny $ tsf (toAny v) w) ))

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
  :: HasProperty s Enclosing
  => ObjectLike s o
  => o
  -> State (World s) (Maybe Enclosing)
getEnclosing e = if isThing e
  then
    defaultPropertyGetter e
  else (do
    o <- getRoom e
    return $ o ^? _Just % objData % roomEnclosing
  )

setEnclosing
  :: HasProperty s Enclosing
  => HasID o
  => o
  -> Enclosing
  -> State (World s) ()
setEnclosing e v = if isThing e
  then
    defaultPropertySetter e v
  else
    modifyRoom e (objData % roomEnclosing .~ v)

getThingLit
  :: ObjectLike s o
  => o
  -> State (World s) (Maybe ThingLit)
getThingLit e = if isThing e
  then (do
    o <- getThing e
    return $ o ^? _Just % objData % thingLit)
  else
    return Nothing -- property that makes no sense if it's a room

setThingLit
  :: HasID o
  => o
  -> ThingLit
  -> State (World s) ()
setThingLit e v = if isThing e
  then
    modifyThing e (objData % thingLit .~ v)
  else
    pass

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

modifyProperty
  :: (o -> State (World s) (Maybe p))
  -> (o -> p -> State (World s) ())
  -> o
  -> (p -> p)
  -> State (World s) ()
modifyProperty g s o f = do
  e <- g o
  when (isNothing e) (do
    logVerbose "Trying to modify a property of an object which does not exist"
    pass)
  whenJust e (s o . f)

defaultPropertySetter
  :: HasProperty ObjectSpecifics v
  => HasProperty s v
  => HasID o
  => o
  -> v -> State (World s) ()
defaultPropertySetter e v = modifyObject e (objSpecifics % propertyL .~ v)

defaultPropertyGetter
  :: HasProperty ObjectSpecifics v
  => HasProperty s v
  => ObjectLike s o
  => o
  -> State (World s) (Maybe v)
defaultPropertyGetter e = do
  o <- getObject e
  return $ preview (objSpecifics % propertyL) =<< o

makeSpecificsWithout [GetX, SetX] ''Enclosing
makeSpecificsWithout [] ''Container
makeSpecificsWithout [] ''Enterable
--makeSpecificsWithout [GetX, SetX] ''ThingLit --TODO: flag

getLocation
  :: ObjectLike s o
  => o
  -> State (World s) (Maybe Entity)
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

getLocation'
  ::  ObjectLike s o
  => o
  -> World s
  -> Maybe Entity
getLocation' o = evalState (getLocation o)

move
  :: HasProperty s Enclosing
  => Entity
  -> Entity
  -> State (World s) Bool
move eObj eLoc = do
  -- reify both objects
  obj <- getThing eObj
  loc <- getEnclosing eLoc
  f <- maybeOrReport2 obj loc
        (logError "Could not find object to move.")
        (logError "Could not find enclosing part of location.")
        (\o' _ -> do
          -- obtain the current container of the thing
          let container = o' ^. containedBy
          oName <- gets $ objectName o'
          contName <- gets $ objectName container
          eLocName <- gets $ objectName eLoc
          logVerbose $ "Moving " <> oName <> " from " <> contName <> " to " <> eLocName

          -- update the old location
          container `noLongerContains` o'

          -- update the thing moved
          eLoc `nowContains` o'
        )
  return $ isJust f

noLongerContains
  :: HasProperty s Enclosing
  => ObjectLike s cont
  => ObjectLike s obj
  => cont
  -> obj
  -> State (World s) ()
noLongerContains cont obj = modifyEnclosing cont
  (enclosingContains %~ ES.delete (getID obj))

nowContains
  :: HasProperty s Enclosing
  => ObjectLike s cont
  => ObjectLike s obj
  => cont
  -> obj
  -> State (World s) ()
nowContains cont obj = do
  modifyEnclosing cont (enclosingContains %~ ES.insert (getID obj))
  modifyThing obj (objData % thingContainedBy .~ getID cont)

isOpaqueClosedContainer
  :: HasProperty s Container
  => ObjectLike s o
  => o
  -> World s
  -> Bool
isOpaqueClosedContainer e w = maybe False (\c -> (_containerOpacity c == Opaque)
    && (_containerOpenable c == Closed)) (getContainer' e w)