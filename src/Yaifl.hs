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
  , module Yaifl.Objects
  --, module Yaifl.Activities
  , newWorld
  , blankWorld
) where

import Yaifl.Common
import Relude
import Yaifl.Objects
--import Yaifl.Actions
import Yaifl.Rulebooks
import Yaifl.Messages
--import Yaifl.Activities

newWorld
  :: [World u r c -> World u r c]
  -> World u r c
newWorld l = flipfoldl' (.) id l blankWorld

blankWorld :: World u r c
blankWorld = World
  { _title = "Untitled"
  , _entityCounter = Entity 0
  , _globalTime = 0
  , _darknessWitnessed = False
  , _firstRoom = Nothing
  , _roomDescriptions = SometimesAbbreviatedRoomDescriptions
  , _things = emptyStore
  , _concepts = emptyStore
  , _rooms = emptyStore
  , _messageBuffers = (emptyMessageBuffer, emptyMessageBuffer)
  , _actions = emptyStore
  , _activities = defaultActivities
  , _whenPlayBegins = whenPlayBeginsRules
  , _actionProcessing = defaultActionProcessingRules
  }