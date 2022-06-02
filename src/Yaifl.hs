-- ~\~ language=Haskell filename=src/Yaifl.hs
-- ~\~ begin <<lit/construction.md|src/Yaifl.hs>>[0] project://lit/construction.md:41

module Yaifl
  (
    --module Yaifl.Common
 -- , module Yaifl.Messages
--  , module Yaifl.Rulebooks
 -- , module Yaifl.Properties
  --, module Yaifl.Activities
    newWorld
  , blankWorld
  , HasStandardProperties
  --, PlainWorldModel
  ) where
import Solitude

import Yaifl.World
import Solitude
import qualified Data.Map as DM
import Yaifl.Common
import Yaifl.Say
--import Yaifl.Rulebooks.ActionProcessing
import Yaifl.Properties.Property
import Yaifl.Properties.Openable
import Yaifl.Properties.Container
import Yaifl.Properties.Enclosing
import Yaifl.Game
--import Yaifl.Objects.Create
--import Yaifl.Rulebooks.WhenPlayBegins
--import Yaifl.ActivityCollection
--import Yaifl.Directions

newWorld :: 
  a --Game s v
  -> a --Game s (World s)
newWorld = id -- modify addBaseActions >> addBaseObjects >> g >> get

--type PlainWorldModel = 'WorldModel () Direction () ()

type HasStandardProperties s = (
  WMHasProperty s Enclosing
  , WMHasProperty s Container
  , WMHasProperty s Enterable
  , WMHasProperty s Openable)

blankWorld :: World (s :: WorldModel)
blankWorld = World
  { _worldMetadata = blankMetadata
  , _worldStores = blankStores
  , _worldActions = blankActions
  , _messageBuffer = blankMessageBuffer
  }

blankActions :: WorldActions s
blankActions = WorldActions
  { _actions = () --DM.empty
  , _activities = ()--_wb
  , _whenPlayBegins = () --_wc
  , _actionProcessing = () --_wd
  }

blankStores :: WorldStores s
blankStores = WorldStores
  { _entityCounter = (Entity 1, Entity (-1))
  , _things = emptyStore
  , _rooms = emptyStore
  , _values = DM.empty
  , _concepts = ()
  }

blankMetadata :: Metadata s
blankMetadata = Metadata 
  { _title = "Untitled"
  , _roomDescriptions = SometimesAbbreviatedRoomDescriptions
  , _dirtyTime = False
  , _globalTime = 0
  , _darknessWitnessed = False
  , _currentPlayer = Entity 1
  , _currentStage = Construction
  , _previousRoom = defaultVoidID
  , _firstRoom = defaultVoidID
  }
{-
  , _activities = defaultActivities
  , _whenPlayBegins = whenPlayBeginsRules
  , _actionProcessing = defaultActionProcessingRules
  }-}
-- ~\~ end
