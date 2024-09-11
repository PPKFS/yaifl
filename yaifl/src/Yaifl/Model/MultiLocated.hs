module Yaifl.Model.MultiLocated
  ( MultiLocated(..)
  , getMultiLocatedMaybe
  , setMultiLocated
  , modifyMultiLocated
  ) where

import Yaifl.Prelude

import Yaifl.Model.Effects
import Yaifl.Model.Entity
import Yaifl.Model.HasProperty
import Yaifl.Model.Query
import Yaifl.Model.TH
import Yaifl.Model.Kinds.AnyObject

import qualified Data.Set as S

newtype MultiLocated = MultiLocated
  { locations :: S.Set EnclosingEntity
  } deriving newtype (Eq, Ord, Read, Show)
    deriving stock (Generic)

makeFieldLabelsNoPrefix ''MultiLocated
makeSpecificsWithout [] ''MultiLocated
