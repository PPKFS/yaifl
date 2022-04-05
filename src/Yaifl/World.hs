module Yaifl.World where

import Solitude
import Yaifl.Common
import Yaifl.Rulebooks
import Yaifl.Logger

-- | Again lifted directly from Inform; this sets whether to always print room
-- descriptions (No..) even if the room is visited, to only print them on the first
-- entry (Sometimes..) or never.
data RoomDescriptions
  = SometimesAbbreviatedRoomDescriptions
  | AbbreviatedRoomDescriptions
  | NoAbbreviatedRoomDescriptions
  deriving (Eq, Show)

type MonadWorld s m = (MonadReader (World s) m, MonadState (World s) m, Logger m)

data World s = World
  { _title :: !Text
  , _entityCounter :: !(Entity, Entity)
  , _dirtyTime :: !Bool
  , _globalTime :: !Timestamp
  , _darknessWitnessed :: !Bool
  , _roomDescriptions :: !RoomDescriptions
  , _worldModel :: s
  , _previousRoom :: !Entity
  , _currentPlayer :: !Entity
  , _firstRoom :: !(Maybe Entity)
  
  --, _directions :: !(Store Direction)
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  , _actions :: !(Map Text (Action s))
  , _activities :: !(ActivityCollection s)
  , _whenPlayBegins :: !(Rulebook s () () Bool)
  , _messageBuffers :: !(MessageBuffer, MessageBuffer)
  , _actionProcessing :: !(forall m. MonadWorld s m => Action s -> UnverifiedArgs s -> m (Maybe Bool))
  }