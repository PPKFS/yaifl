module Yaifl.Std.Kinds.Animal
  ( isAnimal
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.ObjectLike

isAnimal ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isAnimal = objectIsKind "animal"