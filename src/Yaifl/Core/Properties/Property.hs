-- ~\~ language=Haskell filename=src/Yaifl/Core/Properties/Property.hs
-- ~\~ begin <<lit/properties.md|src/Yaifl/Core/Properties/Property.hs>>[0] project://lit/properties.md:6
{-# LANGUAGE DefaultSignatures #-}

module Yaifl.Core.Properties.Property 
  ( HasProperty(..)
  , WMHasProperty
  ) where

import Solitude ( Either(..), const, atraversal, eitherJoin, AffineTraversal' )
import Yaifl.Core.Common ( WMObjSpecifics )

-- | A helper to define that a world model `wm` has a Property.
type WMHasProperty wm v = HasProperty (WMObjSpecifics wm) v

class HasProperty o v where
  default propertyL :: AffineTraversal' o v
  propertyL = atraversal Left const
  propertyL :: AffineTraversal' o v

instance (HasProperty a v, HasProperty b v) => HasProperty (Either a b) v where
  propertyL = propertyL `eitherJoin` propertyL

instance HasProperty () a
-- ~\~ end
