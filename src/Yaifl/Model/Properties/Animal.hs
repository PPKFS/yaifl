module Yaifl.Model.Properties.Animal where

import Solitude
import Yaifl.Model.Objects.Query
import Yaifl.Model.Object
import Yaifl.Model.Objects.Effects

isAnimal ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isAnimal o = getObject o >>= (`isType` "animal")