module Yaifl.Std.Kinds.Animal
  ( isAnimal
  ) where

import Yaifl.Prelude

import Yaifl.Core.Effects
import Yaifl.Core.ObjectLike

isAnimal ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isAnimal = objectIsKind "animal"