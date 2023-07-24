module Yaifl.Model.Properties.Supporter
  ( isSupporter
  ) where

import Solitude

import Yaifl.Model.Object
import Yaifl.Model.Objects.Query


isSupporter ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isSupporter o = getObject o >>= (`isType` "supporter")