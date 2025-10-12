{-|
Module      : Yaifl.Core.HasProperty
Copyright   : (c) Avery 2023-2025
License     : MIT
Maintainer  : ppkfs@outlook.com

Optics for accessing a property from the sum type of object specifics.
-}

module Yaifl.Core.HasProperty (
  -- * Has
    MayHaveProperty(..)
  , HasProperty(..)
  , WMWithProperty
  ) where

import Yaifl.Prelude
import Yaifl.Core.WorldModel

-- | An `AffineTraversal` is an optic that focuses on 0-1 objects; it's a `Prism` without
-- the condition that you can build it back up again..which works great for the possibility
-- that our world model instantiation of object specifics may contain many possible pathways
-- for any individual property but there's no way to do the Prism review.
--
-- This typeclass corresponds to a loose optic/cast.
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

class HasProperty w o v where
  propertyL :: w -> Lens' o v
  default propertyL :: MayHaveProperty o v => w -> Lens' o v
  propertyL _ = lens (fromMaybe (error "property witness was violated") . preview propertyAT) (flip (set propertyAT))