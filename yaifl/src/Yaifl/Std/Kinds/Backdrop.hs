module Yaifl.Std.Kinds.Backdrop
  ( isBackdrop
  , Backdrop(..)
  ) where

import Yaifl.Prelude

import Yaifl.Core.Effects
import Yaifl.Core.ObjectLike
import Yaifl.Std.Kinds.MultiLocated (MultiLocated)

newtype Backdrop = Backdrop MultiLocated
  deriving newtype (Show, Eq, Ord, Generic, Read)
isBackdrop ::
  NoMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isBackdrop = objectIsKind "backdrop"