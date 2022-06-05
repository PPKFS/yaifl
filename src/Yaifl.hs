-- ~\~ language=Haskell filename=src/Yaifl.hs
-- ~\~ begin <<lit/construction.md|src/Yaifl.hs>>[0] project://lit/construction.md:6

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
  , PlainWorldModel
  , Game
  , runGame
  ) where
import Solitude

import Yaifl.World
import qualified Data.Map as DM
import Yaifl.Common
import Yaifl.Say
--import Yaifl.Rulebooks.ActionProcessing
import Yaifl.Properties.Property
import Yaifl.Properties.Openable
import Yaifl.Properties.Container
import Yaifl.Properties.Enclosing
import Yaifl.Directions
import Yaifl.Logger
import Cleff.State hiding ( zoom )
import Yaifl.Objects.Create
import Yaifl.Objects.Query
import Display
--import Yaifl.Objects.Create
--import Yaifl.Rulebooks.WhenPlayBegins
--import Yaifl.ActivityCollection
--import Yaifl.Directions

type PlainWorldModel = 'WorldModel () Direction () ()

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

type EffStack wm = '[Log, ObjectQuery wm, State (Metadata wm), ObjectCreation wm, IOE]
type EffStackNoIO wm = '[Log, ObjectQuery wm, State (Metadata wm), ObjectCreation wm]
type Game wm = Eff (EffStack wm) 

type UnderlyingEffStack wm = '[State (World wm), IOE] 

newWorld :: 
  WMHasProperty wm Enclosing
  => Eff (EffStack wm) ()
newWorld = do
  addBaseObjects
  pass
  {- addBaseActions >> -} 

convertToUnderlyingStack :: 
  forall wm. Eff (EffStack wm)
  ~> Eff (UnderlyingEffStack wm) 
convertToUnderlyingStack = 
  runCreationAsLookup
  . (zoom worldMetadata) 
  . runQueryAsLookup
  . runAndIgnoreLogging
  . raiseUnderN @(State (World wm)) @(EffStackNoIO wm) @('[IOE])

runGame :: 
  Text 
  -> Eff (EffStack wm) a
  -> IO (World wm)
runGame t f = do
  (r, w) <- runIOE $ runState blankWorld $ convertToUnderlyingStack f
  return w

-- ~\~ end
