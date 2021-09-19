{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}
module Yaifl
(
    module Yaifl.Common
  , module Yaifl.Messages
  , module Yaifl.Rulebooks 
  --, module Yaifl.Activitiesd
  , newWorld
  , blankWorld
) where

import Yaifl.Common
import Relude
--import Yaifl.TH
--import Yaifl.Components
--import Yaifl.Actions
import Yaifl.Rulebooks
import Yaifl.Messages
--import Yaifl.Activities

newWorld :: [World u r c -> World u r c] -> World u r c
newWorld l = flipfoldl' (.) id l blankWorld

blankWorld :: World u r c
blankWorld = World "Untitled" Nothing (Entity 0) False SometimesAbbreviatedRoomDescriptions emptyStore emptyStore emptyStore (emptyMessageBuffer, emptyMessageBuffer) 0 emptyStore defaultActivities whenPlayBeginsRules defaultActionProcessingRules

--rulebooks :: Has w (Rulebook w) => Lens' w (Store (Rulebook w))
--rulebooks = store (Proxy :: Proxy (Rulebook w))

