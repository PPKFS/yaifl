module Yaifl.Prelude
  ( Pointed(..)
  , module Solitude
  , module Effectful.Optics
  , module Named
  , module Data.Text.Display
  ) where

import Solitude
import Effectful.Optics
import Named hiding (Name)
import Data.Text.Display hiding (Opaque)

-- | Pointed set class; Monoid without the operation, or the dreaded default typeclass.
class Pointed s where
  identityElement :: s

instance {-# OVERLAPPABLE #-} Monoid m => Pointed m where
  identityElement = mempty