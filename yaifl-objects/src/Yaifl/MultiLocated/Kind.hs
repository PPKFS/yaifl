module Yaifl.MultiLocated.Kind
  ( MultiLocated(..)
  , getMultiLocatedMaybe
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.TH

import qualified Data.Set as S
import Yaifl.Property.Query

newtype MultiLocated = MultiLocated
  { locations :: S.Set EnclosingEntity
  } deriving newtype (Eq, Ord, Read, Show)
    deriving stock (Generic)

makeFieldLabelsNoPrefix ''MultiLocated
makeGetMaybe ''MultiLocated