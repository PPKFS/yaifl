{-|
Module      : Yaifl.Properties.Query
Description : An analog of `Yaifl.Objects.Query` but for properties from objects. Mostly these are helper functions.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Properties.Query
  ( getEnclosing
  , setEnclosing
  , modifyEnclosing

  , objectName
  
  , getOpenable
  
  , getContainer

  , getWornBy
  , getThingLit
  ) where

import Yaifl.Objects.Missing
import Yaifl.WorldInfo
import Yaifl.Properties.Property
import Yaifl.Properties.Enclosing
import Yaifl.Objects.Object
import Solitude
import Yaifl.Common
import Yaifl.Objects.ObjectData
import Yaifl.ObjectSpecifics
import Yaifl.Objects.Query
import Yaifl.Properties.TH
import Yaifl.Properties.Container
import Yaifl.Properties.Openable
import Yaifl.Logger


objectName :: 
  NoMissingObjects m
  => MonadWorld wm m
  => ObjectLike wm o
  => o
  -> m Text
objectName o = do
  o' <- getObject o
  return $ _objName o'

defaultPropertySetter :: 
  MonadWorld wm m
  => HasProperty ObjectSpecifics v
  => WMHasProperty wm v
  => HasID o
  => o
  -> v 
  -> m ()
defaultPropertySetter e v = modifyObject e (objSpecifics % propertyL .~ v)

defaultPropertyGetter :: 
  NoMissingObjects m
  => MonadWorld wm m
  => HasProperty ObjectSpecifics v
  => WMHasProperty wm v
  => ObjectLike wm o
  => o
  -> m (Maybe v)
defaultPropertyGetter e = do
  o <- getObject e
  return $ preview (objSpecifics % propertyL) o

modifyProperty :: 
  MonadWorld wm m
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

-- * Enclosing

getEnclosing :: 
  NoMissingObjects m
  => MonadWorld wm m
  => WMHasProperty wm Enclosing
  => ObjectLike wm o
  => o
  -> m (Maybe Enclosing)
getEnclosing e = if isThing e
  then
    defaultPropertyGetter e
  else (do
    o <- getRoom e
    return $ Just $ o ^. objData % roomEnclosing
  )

setEnclosing :: 
  MonadWorld wm m
  => WMHasProperty wm Enclosing
  => HasID o
  => o
  -> Enclosing
  -> m ()
setEnclosing e v = if isThing e
  then
    defaultPropertySetter e v
  else
    modifyRoom e (objData % roomEnclosing .~ v)

makeSpecificsWithout [GetX, SetX] ''Enclosing
makeSpecificsWithout [] ''Container
makeSpecificsWithout [] ''Enterable
makeSpecificsWithout [] ''Openable

-- * ThingLit

getThingLit :: 
  NoMissingObjects m
  => MonadWorld wm m
  => ObjectLike wm o
  => o
  -> m (Maybe ThingLit)
getThingLit e = if isThing e
  then (do
    o <- getThing e
    return $ Just $ o ^. objData % thingLit)
  else
    return Nothing -- property that makes no sense if it's a room

setThingLit :: 
  MonadWorld wm m
  => HasID o
  => o
  -> ThingLit
  -> m ()
setThingLit e v = if isThing e
  then
    modifyThing e (objData % thingLit .~ v)
  else
    pass

-- todo: should this be maybe maybe? the first for it not being a thing, the second for it not being worn?
getWornBy :: 
  NoMissingObjects m
  => MonadWorld wm m
  => ObjectLike wm o
  => o
  -> m (Maybe Entity)
getWornBy e = if isThing e
  then (do
    o <- getThing e
    return $ o ^? objData % thingWearable % _Wearable % _Just)
  else
    return Nothing -- property that makes no sense if it's a room

setWornBy :: 
  MonadWorld wm m
  => HasID o
  => o
  -> Maybe Entity
  -> m ()
setWornBy e v = if isThing e
  then
    modifyThing e (objData % thingWearable % _Wearable .~ v)
  else
    pass
