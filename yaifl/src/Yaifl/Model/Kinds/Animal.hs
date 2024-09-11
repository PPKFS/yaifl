module Yaifl.Model.Kinds.Animal where

import Yaifl.Prelude
import Yaifl.Model.Kinds.Object
import Yaifl.Model.Effects
import Yaifl.Model.Query
import Yaifl.Model.Metadata

isAnimal ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isAnimal o = getObject o >>= (`isKind` "animal")