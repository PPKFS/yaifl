{-|
Module      : Yaifl.Properties.Property
Description : The basics of the property system.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE DefaultSignatures #-}

module Yaifl.Properties.Property 
  ( HasProperty(..)
  , WMHasProperty
  ) where
    
import Solitude
import Yaifl.Common

-- | A helper to define that a world model `wm` has a Property.
type WMHasProperty wm v = HasProperty (WMObjSpecifics wm) v

class HasProperty o v where
  default propertyL :: AffineTraversal' o v
  propertyL = atraversal Left const
  propertyL :: AffineTraversal' o v

instance (HasProperty a v, HasProperty b v) => HasProperty (Either a b) v where
  propertyL = propertyL `eitherJoin` propertyL

instance HasProperty () a