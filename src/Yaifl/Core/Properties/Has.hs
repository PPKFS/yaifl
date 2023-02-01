{-|
Module      : Yaifl.Core.Properties.Has
Description : A typeclass for when a component has a specific property.
Copyright   : (c) Avery 2022-2023
License     : MIT
Maintainer  : ppkfs@outlook.com

This is mostly so we can have composite components (e.g. a `Yaifl.Core.Objects.ObjectData.RoomData` has an
`Yaifl.Core.Properties.Enclosing`, but also a bunch of other stuff) as well as being generic over different
components with common properties (any `Yaifl.Core.Properties.Enclosing` works, be it a room or a container).
-}

{-# LANGUAGE DefaultSignatures #-}

module Yaifl.Core.Properties.Has (
  -- * Has
    HasProperty(..)
  , WMHasProperty
  ) where

import Solitude
import Yaifl.Core.WorldModel ( WMObjSpecifics )

-- | An `AffineTraversal` is an optic that focuses on 0-1 objects; it's a `Prism` without
-- the condition that you can build it back up again.
class HasProperty o v where
  default propertyL :: AffineTraversal' o v
  propertyL = atraversal Left const
  propertyL :: AffineTraversal' o v

instance (HasProperty a v, HasProperty b v) => HasProperty (Either a b) v where
  propertyL = propertyL `eitherJoin` propertyL

instance HasProperty a v => HasProperty (Maybe a) v where
  propertyL = atraversal (\case
    Nothing -> Left Nothing
    Just x -> case x ^? propertyL of
      Nothing -> Left $ Just x
      Just y -> Right y)
    (\case
      Nothing -> const Nothing
      Just a -> \v -> Just $ a & propertyL .~ v)

-- | A helper to define that a world model `wm` has a Property.
type WMHasProperty wm v = HasProperty (WMObjSpecifics wm) v
