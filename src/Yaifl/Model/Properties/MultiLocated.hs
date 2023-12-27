module Yaifl.Model.Properties.MultiLocated
  ( MultiLocated(..)
  , getMultiLocatedMaybe
  , setMultiLocated
  , modifyMultiLocated
  ) where

import Solitude

import Yaifl.Model.Object
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Properties.TH

import qualified Data.Set as S

newtype MultiLocated = MultiLocated
  { locations :: S.Set EnclosingEntity
  } deriving newtype (Eq, Ord, Read, Show)
    deriving stock (Generic)

makeFieldLabelsNoPrefix ''MultiLocated
makeSpecificsWithout [] ''MultiLocated
