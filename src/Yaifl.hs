{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}
module Yaifl
(
    module Yaifl.Common
  , module Yaifl.TH
  , module Yaifl.Actions
  , module Yaifl.Components
  , module Yaifl.Rulebooks 
  , module Yaifl.Activities
) where

import Yaifl.Common
import Yaifl.TH
import Yaifl.Components
import Yaifl.Actions
import Yaifl.Rulebooks 
import Yaifl.Activities
--rulebooks :: Has w (Rulebook w) => Lens' w (Store (Rulebook w))
--rulebooks = store (Proxy :: Proxy (Rulebook w))

