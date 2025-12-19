module Yaifl.Std.Kinds.Backdrop
  ( isBackdrop
  , Backdrop(..)
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.ObjectLike
import Yaifl.Std.Kinds.MultiLocated (MultiLocated)

newtype Backdrop = Backdrop MultiLocated
  deriving newtype (Show, Eq, Ord, Generic, Read)
isBackdrop ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isBackdrop = objectIsKind "backdrop"