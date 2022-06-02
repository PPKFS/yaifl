module Yaifl.Properties.Query where

import Yaifl.Objects.Query
import Yaifl.Properties.Property ( HasProperty(..), WMHasProperty )
import Yaifl.Properties.Enclosing ( Enclosing )
import Solitude
import Yaifl.Common ( isThing, HasID, getID )
import Yaifl.Objects.Object ( objData, objSpecifics )
import Yaifl.Objects.ObjectData ( roomEnclosing )
import Yaifl.Objects.Specifics ( ObjectSpecifics )
import Cleff.Error (note, Error)

getPropertyOrThrow :: 
  HasID i
  => Error MissingObject :> es
  => Text
  -> i
  -> Maybe v
  -> Eff es v
getPropertyOrThrow t o = note $ MissingObject ("Could not find " <> t) (getID o)

defaultPropertySetter :: 
  NoMissingObjects wm es
  => HasProperty ObjectSpecifics v
  => WMHasProperty wm v
  => ObjectLike wm o
  => o
  -> v 
  -> Eff es ()
defaultPropertySetter e v = modifyObject e (objSpecifics % propertyL .~ v)

defaultPropertyGetter :: 
  NoMissingObjects wm es
  => HasProperty ObjectSpecifics v
  => WMHasProperty wm v
  => ObjectLike wm o
  => o
  -> Eff es (Maybe v)
defaultPropertyGetter e = do
  o <- getObject e
  return $ preview (objSpecifics % propertyL) o

getEnclosing :: 
  NoMissingObjects wm es
  => WMHasProperty wm Enclosing
  => ObjectLike wm o
  => o
  -> Eff es (Maybe Enclosing)
getEnclosing e = if isThing e
  then
    defaultPropertyGetter e
  else (do
    o <- getRoom e
    return $ Just $ o ^. objData % roomEnclosing
  )

setEnclosing :: 
  NoMissingObjects wm es
  => WMHasProperty wm Enclosing
  => ObjectLike wm o
  => o
  -> Enclosing
  -> Eff es ()
setEnclosing e v = if isThing e
  then
    defaultPropertySetter e v
  else
    modifyRoom e (objData % roomEnclosing .~ v)