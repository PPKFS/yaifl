{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}
module Yaifl
(
    module Yaifl.Common
  , module Yaifl.Say
) where

import Yaifl.Common
import Yaifl.Say

--rulebooks :: Has w (Rulebook w) => Lens' w (Store (Rulebook w))
--rulebooks = store (Proxy :: Proxy (Rulebook w))

