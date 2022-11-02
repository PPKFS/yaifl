module Yaifl.Lamp.Properties.Supporter
  ( isSupporter
  ) where

import Yaifl.Core.Object
import Yaifl.Core.Objects.Query
import Solitude
import Effectful

isSupporter ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isSupporter o = getObject o >>= (`isType` "supporter")