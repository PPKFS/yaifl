{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}

module Yaifl.WorldBuilder
(
    WorldBuildInfo(..)
) where

import Relude
import Yaifl.Common
import Lens.Micro.Platform
data WorldBuildInfo w = WBI
    {
        _currentRoom :: Entity,
        _rulebookCache :: RulebookCache w
    } deriving Show

data RulebookCache w = RulebookCache
    {
        _whenPlayBeginsRules :: UncompiledRulebook w (),
        _dummy2 :: Int
    } deriving Show

makeLenses ''WorldBuildInfo
makeLenses ''RulebookCache

