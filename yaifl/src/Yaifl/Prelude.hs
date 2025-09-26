{-# LANGUAGE DataKinds #-}

module Yaifl.Prelude
  ( Pointed(..)
  , module Solitude
  , module Effectful.Optics
  , module Named
  , module Data.Text.Display
  , whileM
  , And
  ) where

import Solitude
import Effectful.Optics
import Named hiding (Name)
import Data.Text.Display hiding (Opaque)

-- | Pointed set class; Monoid without the operation, or the dreaded default typeclass.
class Pointed s where
  identityElement :: s
{-}
instance {-# OVERLAPPABLE #-} Monoid m => Pointed m where
  identityElement = mempty
-}
instance Pointed () where
  identityElement = ()

whileM :: Monad m => (a -> Bool) -> m a -> m a
whileM pr f = do
  a <- f
  if pr a then whileM pr f else return a


type And :: ([k] -> Constraint) -> ([k] -> Constraint) -> [k] -> Constraint

class And c1 c2 l
instance (c1 l, c2 l) => And c1 c2 l