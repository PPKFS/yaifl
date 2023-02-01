module Yaifl.Core.Properties.Query (
  getPropertyOrThrow
  , defaultPropertyGetter
  , defaultPropertySetter
  , modifyProperty
  , getEnclosing
  , setEnclosing
) where

import Solitude

import Yaifl.Core.Entity ( HasID, getID )
import Yaifl.Core.Object
import Yaifl.Core.Objects.Query
import Yaifl.Core.Objects.RoomData
import Yaifl.Core.Properties.Enclosing ( Enclosing )
import Yaifl.Core.Properties.Has ( HasProperty(..), WMHasProperty )
import Effectful.Error.Static ( Error, throwError )

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
  NoMissingObjects wm es
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
getEnclosing e = if isThing (getID e)
  then
    defaultPropertyGetter e
  else (do
    o <- getRoom e
    return $ Just $ o ^. #objectData % #enclosing
  )

setEnclosing ::
  NoMissingObjects wm es
  => WMHasProperty wm Enclosing
  => ObjectLike wm o
  => o
  -> Enclosing
  -> Eff es ()
setEnclosing e v = if isThing (getID e)
  then
    defaultPropertySetter e v
  else
    modifyRoom e (#objectData % #enclosing .~ v)