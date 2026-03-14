module Yaifl.MultiLocated.Kind
  ( MultiLocated(..)
  , getMultiLocatedMaybe
  ) where

import Yaifl.Prelude

import Yaifl.Entity
import Yaifl.TH

import qualified Data.Set as S

newtype MultiLocated = MultiLocated
  { locations :: S.Set EnclosingEntity
  } deriving newtype (Eq, Ord, Read, Show)
    deriving stock (Generic)

makeFieldLabelsNoPrefix ''MultiLocated
makeGetMaybe ''MultiLocated