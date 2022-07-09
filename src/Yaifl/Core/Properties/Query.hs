module Yaifl.Core.Properties.Query where

import Yaifl.Core.Objects.Query
import Yaifl.Core.Properties.Property ( HasProperty(..), WMHasProperty )
import Yaifl.Core.Properties.Enclosing ( Enclosing )

import Yaifl.Core.Common ( isThing, HasID, getID )
import Yaifl.Core.Objects.Object ( objData, objSpecifics )
import Yaifl.Core.Objects.ObjectData ( roomEnclosing )
import Yaifl.Core.Objects.Specifics ( ObjectSpecifics )
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