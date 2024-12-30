module Yaifl.Core.Query.Region
  ( areInRegion
  , isInRegion
  , isSubregionOf

  ) where

import Yaifl.Prelude
import Breadcrumbs

import Yaifl.Core.Metadata
import Yaifl.Core.Kinds.Object
import Yaifl.Core.Effects
import Yaifl.Core.Entity
import Yaifl.Core.ObjectLike
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Kinds.Room
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Tag

import qualified Data.EnumSet as ES
import qualified Data.Set as S
import Yaifl.Model.Kinds.Region
import Yaifl.Core.HasProperty
import Yaifl.Core.Kinds.Enclosing
import Effectful.Error.Static (Error, throwError)
import Yaifl.Core.Refreshable
import Yaifl.Model.WorldModel


areInRegion ::
  NoMissingObjects wm es
  => Foldable f
  => f RoomEntity
  -> RegionEntity
  -> Eff es ()
areInRegion f r = mapM_ (`isInRegion` r) f

isInRegion ::
  NoMissingObjects wm es
  => RoomEntity
  -> RegionEntity
  -> Eff es ()
isInRegion r reg = do
  modifyRegion reg (#rooms %~ S.insert r)
