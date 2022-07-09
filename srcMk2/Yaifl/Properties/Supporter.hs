

module Yaifl.Core.Properties.Supporter
  ( isSupporter

  ) where
import Solitude
import Yaifl.Core.Objects.Object
import Yaifl.Core.WorldInfo

isSupporter ::
  MonadReader (World wm) m
  -- => ObjectLike wm o
  => o
  -> m Bool
isSupporter = (`isType` ObjType "supporter")