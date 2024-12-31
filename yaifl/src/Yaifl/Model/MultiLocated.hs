module Yaifl.Model.MultiLocated
  ( MultiLocated(..)
  , getMultiLocatedMaybe
  , setMultiLocated
  , modifyMultiLocated
  ) where

import Yaifl.Prelude

import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Model.Query
import Yaifl.Model.TH

import qualified Data.Set as S
import Yaifl.Model.WorldModel

newtype MultiLocated = MultiLocated
  { locations :: S.Set EnclosingEntity
  } deriving newtype (Eq, Ord, Read, Show)
    deriving stock (Generic)

makeFieldLabelsNoPrefix ''MultiLocated
makeSpecificsWithout [] ''MultiLocated
