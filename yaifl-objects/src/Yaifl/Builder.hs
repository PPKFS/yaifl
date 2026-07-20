module Yaifl.Builder where

import Yaifl.Prelude

import Yaifl.WorldModel
import GHC.TypeLits


data Purpose
  = Defaults
  | Complete
  deriving stock (Show)

newtype RequiredParameter (a :: Symbol) = RP ()

type family Required (fieldDesc :: Symbol)  (p :: Purpose) a where
  Required f 'Defaults a = RequiredParameter f
  Required f 'Complete a = a

type RequiredName p wm = Required "name" p (WMText wm)
