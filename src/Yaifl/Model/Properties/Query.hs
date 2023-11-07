module Yaifl.Model.Properties.Query (
  getPropertyOrThrow
  , defaultPropertyGetter
  , defaultPropertySetter
  , modifyProperty
  , getEnclosing
  , setEnclosing
  , PropertyLike(..)
  , EnclosingLike
) where

import Solitude

import Yaifl.Model.Entity
import Yaifl.Model.Object
import Yaifl.Model.Objects.Query
import Yaifl.Model.Properties.Enclosing ( Enclosing )
import Yaifl.Model.Properties.Has ( HasProperty(..), WMHasProperty )
import Effectful.Error.Static ( Error, throwError )
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.ObjectLike

getPropertyOrThrow ::
  HasID i
  => Error MissingObject :> es
  => Text
  -> i
  -> Maybe v
  -> Eff es v
getPropertyOrThrow t o = maybe (throwError $ MissingObject ("Could not find " <> t) (getID o)) pure

defaultPropertySetter ::
  NoMissingObjects wm es
  => WMHasProperty wm v
  => ObjectLike wm o
  => o
  -> v
  -> Eff es ()
defaultPropertySetter e v = modifyObject e (#specifics % propertyL .~ v)

defaultPropertyGetter ::
  NoMissingRead wm es
  => WMHasProperty wm v
  => ObjectLike wm o
  => o
  -> Eff es (Maybe v)
defaultPropertyGetter e = do
  o <- getObject e
  return $ preview (#specifics % propertyL) o

modifyProperty ::
  (o -> Eff es (Maybe p))
  -> (o -> p -> Eff es ())
  -> o
  -> (p -> p)
  -> Eff es ()
modifyProperty g s o f = do
  e <- g o
  when (isNothing e) (do
    --logVerbose "Trying to modify a property of an object which does not exist"
    pass)
  whenJust e (s o . f)

getEnclosing ::
  NoMissingObjects wm es
  => WMHasProperty wm Enclosing
  => ObjectLike wm o
  => o
  -> Eff es (Maybe Enclosing)
getEnclosing e = asThingOrRoomM e
  defaultPropertyGetter
  (pure . Just . view (#objectData % #enclosing))

setEnclosing ::
  NoMissingObjects wm es
  => WMHasProperty wm Enclosing
  => ObjectLike wm o
  => o
  -> Enclosing
  -> Eff es ()
setEnclosing e v = asThingOrRoomM e
  (`defaultPropertySetter` v)
  (\o -> modifyRoom o (#objectData % #enclosing .~ v))

asThingKind ::
  o
  -> a
  -> (Object wm s a)
asThingKind = error ""

class PropertyLike wm prop o where
  getAs :: (WMHasProperty wm prop, NoMissingObjects wm es) => o -> Eff es prop

type EnclosingLike wm o = PropertyLike wm Enclosing o

instance PropertyLike wm Enclosing Enclosing where
  getAs = pure

instance PropertyLike wm Enclosing (TaggedEntity EnclosingTag) where
 getAs o = do
    a <- getObject o
    e <- getEnclosing a
    getPropertyOrThrow "enclosing" a e

instance PropertyLike wm Enclosing (TaggedEntity RoomTag) where
 getAs o = do
    a <- getRoom o
    getAs a

instance PropertyLike wm Enclosing (Room wm) where
 getAs o = pure $ o ^. #objectData % #enclosing