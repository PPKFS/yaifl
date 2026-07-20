{-|
Module      : Yaifl.Property.Has
Copyright   : (c) Avery 2023-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Property access optics for Yaifl's object system.

This module provides typeclasses and optics for safely accessing properties
from object specifics sum types. It handles the complexity of working with
sum types where properties may or may not be present.
-}

module Yaifl.Property.Has (
  -- * Has
    MayHaveProperty(..)
  , HasProperty(..)
  , WMWithProperty
  ) where

import Yaifl.Prelude
import Yaifl.WorldModel ( WMObjSpecifics )

-- | Typeclass for optional property access using `AffineTraversal`.
--
-- An `AffineTraversal` focuses on 0-1 targets, making it ideal for property access
-- where a property may or may not exist in an object's specifics sum type.
-- Unlike a `Prism`, it doesn't require the ability to reconstruct the original structure.
--
-- This typeclass provides a way to "cast" or focus on specific properties within
-- complex sum types, handling cases where the property path might not exist.
class MayHaveProperty o v where
  default propertyAT :: AffineTraversal' o v
  propertyAT = atraversal Left const
  propertyAT :: AffineTraversal' o v

instance (MayHaveProperty a v, MayHaveProperty b v) => MayHaveProperty (Either a b) v where
  propertyAT = propertyAT `eitherJoin` propertyAT

instance MayHaveProperty a v => MayHaveProperty (Maybe a) v where
  propertyAT = atraversal (\case
    Nothing -> Left Nothing
    Just x -> case x ^? propertyAT of
      Nothing -> Left $ Just x
      Just y -> Right y)
    (\case
      Nothing -> const Nothing
      Just a -> \v -> Just $ a & propertyAT .~ v)

type WMWithProperty wm v = MayHaveProperty (WMObjSpecifics wm) v

-- | Typeclass for guaranteed property access using `Lens`.
--
-- Provides a `Lens` for properties that are guaranteed to exist in the structure.
-- Unlike `MayHaveProperty`, this assumes the property is always present and
-- provides a stronger guarantee through the type system.
--
-- The default implementation uses `MayHaveProperty` but adds a runtime check
-- that fails if the property invariant is violated.
-- i.e. the assumption is that you do not make instances of this
-- everywhere.
class HasProperty w o v where
  -- | The `Lens` optic for accessing this property.
  -- Requires a witness value to identify which property to focus on.
  propertyL :: w -> Lens' o v

  -- | Default implementation using `MayHaveProperty`.
  -- Fails with error if property doesn't exist (invariant violation).
  default propertyL :: MayHaveProperty o v => w -> Lens' o v
  propertyL _ = lens (fromMaybe (error "property witness was violated") . preview propertyAT) (flip (set propertyAT))