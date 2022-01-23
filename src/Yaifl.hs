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
  , module Yaifl.Properties
  --, module Yaifl.Activities
  , newWorld
  , blankWorld
  , HasStandardProperties
) where

import Yaifl.Common
import Relude
import Yaifl.Properties
import Yaifl.Actions
import Yaifl.Rulebooks
import Yaifl.Messages
import qualified Data.Map.Strict as DM
import Yaifl.Activities

newWorld
  :: HasStandardProperties s
  => Game s v
  -> Game s (World s)
newWorld g = modify addBaseActions >> addBaseObjects >> g >> get

type HasStandardProperties s = (
  HasProperty s Enclosing
  , HasProperty s Container
  , HasProperty s Enterable
  , HasProperty s Openable)
  
blankWorld
  :: HasStandardProperties s
  => World s
blankWorld = World
  { _title = "Untitled"
  , _entityCounter = (Entity 1, Entity (-1))
  , _globalTime = 0
  , _darknessWitnessed = False
  , _firstRoom = Nothing
  , _roomDescriptions = SometimesAbbreviatedRoomDescriptions
  , _currentPlayer = defaultPlayerID
  , _things = blank
  , _concepts = () --emptyStore
  , _rooms = blank
  , _messageBuffers = (emptyMessageBuffer, emptyMessageBuffer)
  , _actions = DM.empty
  , _activities = defaultActivities
  , _whenPlayBegins = whenPlayBeginsRules
  , _actionProcessing = defaultActionProcessingRules
  , _previousRoom = defaultVoidID
  , _dirtyTime = False
  }