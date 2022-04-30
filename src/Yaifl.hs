-- ~\~ language=Haskell filename=src/Yaifl.hs
-- ~\~ begin <<lit/construction.md|src/Yaifl.hs>>[0]
{- |
Copyright: (c) 2020 Avery
SPDX-License-Identifier: MIT
Maintainer: Avery <thecommunistduck@hotmail.co.uk>

Yet another interactive fiction library.
-}
module Yaifl
  (
    --module Yaifl.Common
 -- , module Yaifl.Messages
--  , module Yaifl.Rulebooks
 -- , module Yaifl.Properties
  --, module Yaifl.Activities
    newWorld
  , blankWorld
  --, HasStandardProperties
  --, PlainWorldModel
  ) where
import Solitude
{-
import Yaifl.World
import Solitude
import qualified Data.Map as DM
import Yaifl.Common
import Yaifl.Say
import Yaifl.Rulebooks.ActionProcessing
import Yaifl.Properties.Property
import Yaifl.Properties.Openable
import Yaifl.Properties.Container
import Yaifl.Properties.Enclosing
import Yaifl.Game
import Yaifl.Objects.Create
import Yaifl.Rulebooks.WhenPlayBegins
import Yaifl.ActivityCollection
import Yaifl.Directions

newWorld :: 
  HasStandardProperties s
  => Game s v
  -> Game s (World s)
newWorld g = modify addBaseActions >> addBaseObjects >> g >> get

type PlainWorldModel = 'WorldModel () Direction () ()

type HasStandardProperties s = (
  WMHasProperty s Enclosing
  , WMHasProperty s Container
  , WMHasProperty s Enterable
  , WMHasProperty s Openable)
blankWorld :: 
  HasStandardProperties s
  => World (s :: WorldModel)
blankWorld = World
  { _title = "Untitled"
  , _entityCounter = (Entity 1, Entity (-1))
  , _globalTime = 0
  , _darknessWitnessed = False
  , _firstRoom = Nothing
  , _roomDescriptions = SometimesAbbreviatedRoomDescriptions
  , _currentPlayer = Entity 1
  , _things = emptyStore
  , _concepts = () --emptyStore
  , _rooms = emptyStore
  , _values = DM.empty
  , _messageBuffers = (emptyMessageBuffer, emptyMessageBuffer)
  , _actions = DM.empty
  , _activities = defaultActivities
  , _whenPlayBegins = whenPlayBeginsRules
  , _actionProcessing = defaultActionProcessingRules
  , _previousRoom = defaultVoidID
  , _dirtyTime = False
  , _currentStage = Construction
  }
-}
blankWorld = undefined
newWorld = undefined
-- ~\~ end
