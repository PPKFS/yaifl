{-# LANGUAGE DefaultSignatures #-}

module Yaifl.Model.Properties.Has (
  -- * Has
    MayHaveProperty(..)
  , WMWithProperty
  ) where

import Solitude
import Yaifl.Model.WorldModel ( WMObjSpecifics )

-- | An `AffineTraversal` is an optic that focuses on 0-1 objects; it's a `Prism` without
-- the condition that you can build it back up again.
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

-- | A helper to define that a world model `wm` has a Property.
type WMWithProperty wm v = MayHaveProperty (WMObjSpecifics wm) v