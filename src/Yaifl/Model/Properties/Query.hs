{-|
Module      : Yaifl.Model.Properties.Query
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

Typeclasses for things which are XLike (can be resolved into an X in an @Eff es@ context with relevant
constraints/effects).
-}

module Yaifl.Model.Properties.Query (
  getPropertyOrThrow
  , defaultPropertyGetter
  , defaultPropertySetter
  , modifyProperty
  , getEnclosingMaybe
  , getEnclosing
  , setEnclosing
  , HasProperty(..)
  , EnclosingObject(..)
) where

import Solitude

import Effectful.Error.Static ( Error, throwError )
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Object
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.Query
import Yaifl.Model.Properties.Enclosing ( Enclosing )
import Yaifl.Model.Properties.Has

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

getEnclosingMaybe ::
  forall wm.
  WMWithProperty wm Enclosing
  => AnyObject wm
  -> Maybe Enclosing
getEnclosingMaybe e = asThingOrRoom
  (const $ defaultPropertyGetter e)
  (Just . view (#objectData % #enclosing)) e

setEnclosing ::
  forall wm es o.
  NoMissingObjects wm es
  => WMWithProperty wm Enclosing
  => CanBeAny wm o
  => o
  -> Enclosing
  -> Eff es ()
setEnclosing e v = asThingOrRoom
  (`defaultPropertySetter` v)
  (\o -> modifyRoom o (#objectData % #enclosing .~ v)) (toAny @wm e)

getEnclosing ::
  WMWithProperty wm Enclosing
  => EnclosingEntity
  -> AnyObject wm
  -> Enclosing
getEnclosing _ = fromMaybe (error "property witness was violated") . getEnclosingMaybe

-- | A lens that is guaranteed by witnesses
class HasProperty w o v where
  propertyL :: w -> Lens' o v

instance MayHaveProperty o v => HasProperty w o v where
  propertyL _ = lens (fromMaybe (error "property witness was violated") . preview propertyAT) (flip (set propertyAT))

class EnclosingObject o where
  enclosingL :: Lens' o Enclosing

instance EnclosingObject (Room wm) where
  enclosingL = #objectData % #enclosing

instance WMWithProperty wm Enclosing => EnclosingObject (EnclosingEntity, Thing wm) where
  enclosingL = lens (\(e, o) -> getEnclosing e (toAny o)) (\(e, o) enc -> (e, o & (#specifics % propertyAT .~ enc)))