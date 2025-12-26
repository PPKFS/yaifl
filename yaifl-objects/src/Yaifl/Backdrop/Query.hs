module Yaifl.Backdrop.Query
  ( isBackdrop
  ) where

import Yaifl.Prelude

import Yaifl.Effects.ObjectQuery
import Yaifl.ObjectLike

isBackdrop ::
  WithoutMissingObjects wm es
  => ObjectLike wm o
  => o
  -> Eff es Bool
isBackdrop = objectIsKind "backdrop"
