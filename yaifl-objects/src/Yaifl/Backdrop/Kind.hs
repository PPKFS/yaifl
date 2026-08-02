module Yaifl.Backdrop.Kind
  ( Backdrop(..)
  ) where

import Yaifl.Prelude

import Yaifl.MultiLocated.Kind (MultiLocated)
import Yaifl.Enclosing.Kind
import Yaifl.Region.Kind
import qualified Data.Set as S

data BackdropLocations = InRooms (NonEmpty EnclosingEntity) | InRegions (NonEmpty RegionEntity) | Everywhere
data Backdrop = Backdrop
  { rooms :: MultiLocated
  , regions :: S.Set RegionEntity
  , everywhere :: Bool
  } deriving stock (Show, Eq, Ord, Generic)