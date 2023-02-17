module Yaifl.Lamp.Properties.Animal where

import Solitude
import Yaifl.Core.Objects.Query
import Yaifl.Core.Object

isAnimal ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isAnimal o = getObject o >>= (`isType` "supporter")