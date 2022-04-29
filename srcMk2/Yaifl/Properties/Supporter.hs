

module Yaifl.Properties.Supporter
  ( isSupporter

  ) where
import Solitude
import Yaifl.Objects.Object
import Yaifl.WorldInfo

isSupporter ::
  MonadReader (World wm) m
  -- => ObjectLike wm o
  => o
  -> m Bool
isSupporter = (`isType` ObjType "supporter")