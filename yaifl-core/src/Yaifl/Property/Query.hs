module Yaifl.Property.Query
  ( getPropertyOrThrow
  , defaultPropertyGetter
  , defaultPropertySetter
  , modifyProperty

  ) where

import Yaifl.Prelude

import Effectful.Error.Static (Error, throwError)
import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.Property.Has
import Yaifl.AnyObject
import Yaifl.Object.Kind
import Yaifl.Object.Query

getPropertyOrThrow ::
  HasEntity i
  => Error MissingObject :> es
  => Text
  -> i
  -> Maybe v
  -> Eff es v
getPropertyOrThrow t o = maybe (throwError $ MissingObject ("Could not find " <> t) (getEntity o)) pure

defaultPropertySetter ::
  WithoutMissingObjects wm es
  => WMWithProperty wm v
  => CanBeAny wm o
  => o
  -> v
  -> Eff es ()
defaultPropertySetter e v = modifyObject (toAny e) (#specifics % propertyAT .~ v)

defaultPropertyGetter ::
  forall wm o v.
  WMWithProperty wm v
  => CanBeAny wm o
  => o
  -> Maybe v
defaultPropertyGetter o = preview (#specifics % propertyAT) (toAny o)

modifyProperty ::
  CanBeAny wm o
  => (AnyObject wm -> Maybe p)
  -> (AnyObject wm -> p -> Eff es ())
  -> o
  -> (p -> p)
  -> Eff es ()
modifyProperty g s o f = do
  let e = g (toAny o)
  when (isNothing e) (do
    --logVerbose "Trying to modify a property of an object which does not exist"
    pass)
  whenJust e (s (toAny o) . f)
