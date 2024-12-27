module Yaifl.Model.Kinds.Backdrop
  ( isBackdrop
  , Backdrop(..)
  ) where

import Yaifl.Prelude

import Yaifl.Model.Effects
import Yaifl.Model.ObjectLike
import Yaifl.Model.MultiLocated (MultiLocated)

newtype Backdrop = Backdrop MultiLocated
  deriving newtype (Show, Eq, Ord, Generic, Read)
isBackdrop ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isBackdrop = objectIsKind "backdrop"