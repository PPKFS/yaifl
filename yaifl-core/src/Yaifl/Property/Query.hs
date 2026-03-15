{-|
Module      : Yaifl.Property.Query
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Property query and manipulation utilities.

This module provides utility functions for working with object properties in the game world.
It offers safe property access with proper error handling, property modification functions,
and integration with the property system.

Key components:
- `getPropertyOrThrow`: Get a property or throw an error if missing
- `defaultPropertyGetter`: Get a property from an object's specifics
- `defaultPropertySetter`: Set a property on an object's specifics
- `modifyProperty`: Modify a property using getter/setter functions
-}

module Yaifl.Property.Query
  ( -- * Property Access
    getPropertyOrThrow
  , defaultPropertyGetter
  
  -- * Property Modification
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

-- | Get a property or throw an error if missing.
--
-- This function attempts to get a property value, and if the value is 'Nothing',
-- it throws a 'MissingObject' error with a descriptive message including the property name
-- and the entity that was searched.
getPropertyOrThrow ::
  HasEntity i
  => Error MissingObject :> es
  => Text -- ^ Property name for error messages
  -> i -- ^ Object identifier
  -> Maybe v -- ^ Property value to check
  -> Eff es v
getPropertyOrThrow t o = maybe (throwError $ MissingObject ("Could not find " <> t) (getEntity o)) pure

-- | Get a property from an object's specifics.
--
-- This function uses optics to safely extract a property value from an object's specifics.
-- It returns 'Nothing' if the property doesn't exist or the object doesn't have specifics.
defaultPropertyGetter ::
  forall wm o v.
  WMWithProperty wm v
  => CanBeAny wm o
  => o -- ^ Object to get property from
  -> Maybe v -- ^ Property value if found
defaultPropertyGetter o = preview (#specifics % propertyAT) (toAny o)

-- | Set a property on an object's specifics.
--
-- This function uses optics to set a property value on an object's specifics.
-- It assumes the object exists (enforced by 'WithoutMissingObjects' constraint).
defaultPropertySetter ::
  WithoutMissingObjects wm es
  => WMWithProperty wm v
  => CanBeAny wm o
  => o -- ^ Object to set property on
  -> v -- ^ Property value to set
  -> Eff es ()
defaultPropertySetter e v = modifyObject (toAny e) (#specifics % propertyAT .~ v)

-- | Modify a property using getter/setter functions.
--
-- This function provides a flexible way to modify properties by taking getter and setter
-- functions as arguments. It safely handles the case where the property doesn't exist
-- by doing nothing in that case.
modifyProperty ::
  CanBeAny wm o
  => (AnyObject wm -> Maybe p) -- ^ Getter function
  -> (AnyObject wm -> p -> Eff es ()) -- ^ Setter function
  -> o -- ^ Object to modify
  -> (p -> p) -- ^ Modification function
  -> Eff es ()
modifyProperty g s o f = do
  let e = g (toAny o)
  when (isNothing e) (do
    --logVerbose "Trying to modify a property of an object which does not exist"
    pass)
  whenJust e (s (toAny o) . f)
