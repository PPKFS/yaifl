
{-# LANGUAGE UndecidableInstances #-}

module Yaifl.Model.Properties.MultiLocated
  ( MultiLocated(..)
  , getMultiLocatedMaybe
  , setMultiLocated
  , modifyMultiLocated
  ) where

import qualified Data.Set as S
import Yaifl.Model.Objects.Entity
import Yaifl.Model.Properties.TH
import Yaifl.Model.Properties.Has
import Yaifl.Model.Properties.Query
import Yaifl.Model.Objects.Effects
import Yaifl.Model.Object
import Solitude

newtype MultiLocated = MultiLocated
  { locations :: S.Set EnclosingEntity
  } deriving newtype (Eq, Ord, Read, Show)
    deriving stock (Generic)

makeFieldLabelsNoPrefix ''MultiLocated
makeSpecificsWithout [] ''MultiLocated
