module Yaifl.Backdrop.Kind
  ( Backdrop(..)
  ) where

import Yaifl.Prelude

import Yaifl.MultiLocated.Kind (MultiLocated)

newtype Backdrop = Backdrop MultiLocated
  deriving newtype (Show, Eq, Ord, Generic, Read)