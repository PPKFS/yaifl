module Yaifl.Supporter.Query
  ( isSupporter
  , setSupporter
  , modifySupporter
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.Object.Kind
import Yaifl.Metadata
import Yaifl.Property.Query
import Yaifl.TH
import Yaifl.ObjectLike
import Yaifl.Supporter.Kind

-- | Check if @o@ is of the @supporter@ type.
isSupporter ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isSupporter o = getObject o >>= (`isKind` "supporter")

makeModify ''Supporter