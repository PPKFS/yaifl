module Yaifl.Std.Kinds.MultiLocated
  ( MultiLocated(..)
  , getMultiLocatedMaybe
  , setMultiLocated
  , modifyMultiLocated
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.Entity
import Yaifl.TH

import qualified Data.Set as S
import Yaifl.Core.Query.Property

newtype MultiLocated = MultiLocated
  { locations :: S.Set EnclosingEntity
  } deriving newtype (Eq, Ord, Read, Show)
    deriving stock (Generic)

makeFieldLabelsNoPrefix ''MultiLocated
makeSpecificsWithout [] ''MultiLocated
