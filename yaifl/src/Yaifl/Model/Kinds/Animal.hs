module Yaifl.Model.Kinds.Animal
  ( isAnimal
  ) where

import Yaifl.Prelude

import Yaifl.Model.Effects
import Yaifl.Model.ObjectLike

isAnimal ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isAnimal = objectIsKind "animal"