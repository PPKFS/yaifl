module Yaifl.Supporter.Query
  ( isSupporter
  , setSupporter
  , modifySupporter
  , isNowOn
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.Object.Kind
import Yaifl.Metadata
import Yaifl.Property.Query
import Yaifl.TH
import Yaifl.ObjectLike
import Yaifl.Supporter.Kind
import Yaifl.Effects.RuleEffects
import Yaifl.Enclosing.Kind
import Yaifl.Enclosing.Query
import Yaifl.Move

-- | Check if @o@ is of the @supporter@ type.
isSupporter ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isSupporter o = getObject o >>= (`isKind` "supporter")

makeModify ''Supporter

isNowOn ::
  RuleEffects wm es
  => WMWithProperty wm Enclosing
  => ThingLike wm t
  => t
  -> SupporterEntity
  -> Eff es ()
isNowOn t e = do
  t' <- getThing t
  e' <- getEnclosingObject e
  void $ move t' e'