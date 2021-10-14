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

  -- * get/set/modify
  , containedBy

  , move

  , getThing
  , getObject
  , getThing'
  , getObject'

  , fromAny

  -- * Property stuff
  , HasProperty

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

logObject
  :: ObjectLike o
  => Text
  -> o
  -> State (World s) ()
logObject n e = do
  o <- getObject e
  logVerbose $ n <> "\n" <> prettify o
  whenJust o $ \Object{..} -> logVerbose _objName

objectName
  :: ObjectLike o
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
  :: ObjectLike o
  => StoreLens' s d
  -> o
  -> State (World s) (Maybe (Object s d))
getObjectFrom l e = do
    o <- gets $ getAbstractObjectFrom l e
    mapMaybeM o (state . reifyObject l)

getAbstractObjectFrom
  :: ObjectLike o
  => StoreLens' s d
  -> o
  -> World s
  -> Maybe (AbstractObject s d)
getAbstractObjectFrom l e w = w ^? l % ix (getID e)

getThing
  :: ObjectLike o
  => o
  -> State (World s) (Maybe (Thing s))
getThing = getObjectFrom things

getRoom
  :: ObjectLike o
  => o
  -> State (World s) (Maybe (Room s))
getRoom = getObjectFrom rooms

getAbstractThing
  :: ObjectLike o
  => o
  -> World s
  -> Maybe (AbstractThing s)
getAbstractThing = getAbstractObjectFrom things

getAbstractRoom
  :: ObjectLike o
  => o
  -> World s
  -> Maybe (AbstractRoom s)
getAbstractRoom = getAbstractObjectFrom rooms

getObject
  :: ObjectLike o
  => o
  -> State (World s) (Maybe (AnyObject s))
getObject e = if isThing e
  then
    (do
      o <- getObjectFrom things e
      return $ toAny <$> o)
  else
    (do
      o <- getObjectFrom rooms e
      return $ toAny <$> o)

getThing' 
  :: ObjectLike o 
  => o -> World s 
  -> Maybe (Thing s)
getThing' o = evalState (getThing o)

getObject' 
  :: ObjectLike o 
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
  :: ObjectLike o
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
  :: ObjectLike o
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
  :: ObjectLike o
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

getEnclosing
  :: HasProperty s Enclosing
  => ObjectLike o
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
  => ObjectLike o
  => o
  -> Enclosing
  -> State (World s) ()
setEnclosing e v = if isThing e
  then
    defaultPropertySetter e v
  else
    modifyRoom e (objData % roomEnclosing .~ v)

modifyObjectFrom
  :: ObjectLike o
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
  :: ObjectLike o
  => o
  -> (Thing s -> Thing s)
  -> State (World s) ()
modifyThing = modifyObjectFrom things

modifyRoom
  :: ObjectLike o
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
  => ObjectLike o
  => o
  -> v -> State (World s) ()
defaultPropertySetter e v = modifyObject e (objSpecifics % propertyL .~ v)

defaultPropertyGetter
  :: HasProperty ObjectSpecifics v
  => HasProperty s v
  => ObjectLike o
  => o
  -> State (World s) (Maybe v)
defaultPropertyGetter e = do
  o <- getObject e
  return $ preview (objSpecifics % propertyL) =<< o

makeSpecificsWithout [GetX, SetX] ''Enclosing
makeSpecificsWithout [] ''Container

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
          -- obtain the current location of the thing
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
  => ObjectLike cont
  => ObjectLike obj
  => cont
  -> obj
  -> State (World s) ()
noLongerContains cont obj = modifyEnclosing cont
  (enclosingContains %~ ES.delete (getID obj))

nowContains
  :: HasProperty s Enclosing
  => ObjectLike cont
  => ObjectLike obj
  => cont
  -> obj
  -> State (World s) ()
nowContains cont obj = do
  modifyEnclosing cont (enclosingContains %~ ES.insert (getID obj))
  modifyThing obj (objData % thingContainedBy .~ getID cont)

isOpaqueClosedContainer 
  :: HasProperty s Container
  => ObjectLike o
  => o
  -> World s
  -> Bool
isOpaqueClosedContainer e w = False
  where
    o = getObject' e w
    cont = getContainer' e w
  --return $ c ^? _Just . containerObjData . opacity == Just Opaque && fmap _containerOpenable c == Just Closed


{-






{-


g
  :: World o
  -> AbstractObject o (Property o)
  -> World o
g w o
  | isThingID e = maybe w (\o' -> w & set (things % ix e) o') (projectThing o)
  | isRoomID e = maybe w (\o' -> w & set (rooms % ix e) o') (projectRoom o)
  | isConceptID e = maybe w (\o' -> w & set (concepts % ix e) o') (projectConcept o)
  | otherwise = error $ "Missing case " <> show e
  where e = getID o

--Lens' AO Prop | Maybe AO Thing
projectThing :: AbstractObject o (Property o) -> Maybe (AbstractObject o (ThingData t))
projectThing o = (Just . upd o ThingProperty) =<< (o ^? objectL % objDetails % _ThingProperty)

upd :: AbstractObject o (Property o) -> (a -> Property o) -> a -> AbstractObject o a
upd (StaticObject o) _ t = StaticObject (o & objDetails .~ t)
upd (DynamicObject (TimestampedObject to tt tu)) f t=
  DynamicObject (TimestampedObject (to { _objDetails = t}) tt
    (\o w -> a o w f v tu))

a ::
  Object a
  -> World o
  -> (a -> Property o)
  -> Maybe (Property o -> a)
  -> (Object (Property o) -> World o -> Object (Property o)) -> Object a
a o w inj proj upd' = case proj of
  Just t -> t <$> upd' (inj <$> o) w
  Nothing -> o

projectRoom:: AbstractObject o (Property o) -> Maybe (AbstractObject o (RoomData r))
projectRoom = traverse (preview _RoomProperty)

projectConcept :: AbstractObject o (Property o) -> Maybe (AbstractObject o (ConceptData c))
projectConcept = traverse (preview _ConceptProperty)



-}




{-
enclosing
  :: HasComponent o Enclosing
  => AffineTraversal' (Object (Property o)) Enclosing
enclosing = objDetails % traverseProperty @Enclosing

traverseProperty
  :: forall v o
  .  HasThingTraversal t v
  => HasRoomTraversal r v
  => HasConceptTraversal c v
  => AffineTraversal' (Property o) v
traverseProperty = atraversal
  (\s -> case s of
    ThingProperty t -> maybeToRight s (getThingPart t)
    RoomProperty r -> maybeToRight s (getRoomPart r)
    ConceptProperty c -> maybeToRight s (getConceptPart c))
  (\s b -> case s of
    ThingProperty t -> ThingProperty $ setThingPart t b
    RoomProperty r -> RoomProperty $ setRoomPart r b
    ConceptProperty c -> ConceptProperty $ setConceptPart c b)

class HasPropertyTraversal t v where
  getPart :: t -> Maybe v
  default getPart :: t -> Maybe v
  getPart = const Nothing

  setPart :: t -> v -> t
  default setPart :: t -> v -> t
  setPart = const

-- so for any triple o, and N traversal targets, we have:
-- N copies of HasProperty x, for each of o
-- N copies of HasXTraversal X n - these are /all/ just blank instances
-- except when we don't need a copy (e.g. room enclosing)
-- HasProperty is the special case here
-- I think what I want is to make default instances for everything, except certain things in a list
-- which I then need to make my own ones
instance HasPropertyTraversal () Enclosing
instance HasThingTraversal () Enclosing
instance HasConceptTraversal () Enclosing
-}

    objToMove <- getThing obj
    mloc <- getComponent @Enclosing le -- use $ gameWorld . (store @w @Enclosing) . at le
    locName <- getComponent @(Object w) le
    doIfExists2
        objToMove
        mloc
        (showMaybeObjDebug objToMove <> " has no physical component, so cannot be moved.")
        (showMaybeObjDebug locName <> " has no enclosing component, so cannot move objects into it.")
        ( \o _ -> do
            --todo: recalc the location?
            -- todo: doesn't this mean the location is actually
            -- a derived property?
            let vl = o ^. thingPhysical . enclosedBy
            vlo <- getComponent @(Object w) vl
            logDebug $ "Moving " <> showObjDebug o <> " from " <> showMaybeObjDebug vlo <> " to " <> showMaybeObjDebug locName
            adjustComponent @(Physical w) obj (enclosedBy .~ le)
            adjustComponent @Enclosing vl (encloses %~ DS.delete obj)
            adjustComponent @Enclosing le (encloses %~ DS.insert obj)
            return True
        )
-}