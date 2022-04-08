{-|
Module      : Yaifl.World
Description : The monolithic record state that runs everything.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}

module Yaifl.World
  ( -- * Types
    RoomDescriptions(..)
  , World(..)
  , MonadWorld
   -- * Modifying the world
  , newEntityID
  , setTitle
  , getGlobalTime
  , tickGlobalTime
    -- * Lenses
  , title
  , darknessWitnessed
  , roomDescriptions
  , actionProcessing
  , actions
  , things
  , rooms
  ) where

import Solitude
import Yaifl.Common
import Yaifl.Say
import Yaifl.Rulebooks.Rulebook
import Yaifl.Logger
import Yaifl.Activities
import Yaifl.Actions.Action
import Yaifl.Objects.Dynamic

-- | Again lifted directly from Inform; this sets whether to always print room
-- descriptions (No..) even if the room is visited, to only print them on the first
-- entry (Sometimes..) or never.
data RoomDescriptions
  = SometimesAbbreviatedRoomDescriptions
  | AbbreviatedRoomDescriptions
  | NoAbbreviatedRoomDescriptions
  deriving stock (Eq, Show, Read, Ord, Generic)

-- | A convenient type synonym for a read-write World monad + logging
type MonadWorld wm m = (MonadReader (World wm) m, MonadState (World wm) m, Logger m)

-- | The big one, a `World` is the monolithic record of all state in the game.
-- this includes creation information for the DSL, game specific information, actions
-- and processing rulebooks, as well as the world model (that deals with things and rooms).
data World (wm :: WorldModel) = World
  { _title :: !Text
  , _entityCounter :: !(Entity, Entity)
  , _dirtyTime :: !Bool
  , _globalTime :: !Timestamp
  , _darknessWitnessed :: !Bool
  , _roomDescriptions :: !RoomDescriptions
  , _things :: !(Store (AbstractThing wm))
  , _rooms :: !(Store (AbstractRoom wm))
  --, _directions :: !
  , _values :: !(Map Text (Values wm))
  , _previousRoom :: !Entity
  , _currentPlayer :: !Entity
  , _firstRoom :: !(Maybe Entity)
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  , _actions :: !(Map Text (Action wm))
  , _activities :: !(ActivityCollection wm)
  , _whenPlayBegins :: !(Rulebook wm () () Bool)
  , _messageBuffers :: !(MessageBuffer, MessageBuffer)
  , _actionProcessing :: ActionProcessing wm
  }

makeLenses ''World
makeLenses ''WorldModel

-- | Generate a new entity ID.
newEntityID :: 
  Bool
  -> World o
  -> (Entity, World o)
newEntityID True = entityCounter % _1 <<+~ 1
newEntityID False = entityCounter % _2 <<-~ 1

-- | Update the game title.
setTitle :: 
  MonadWorld s m
  => Text -- ^ New title.
  -> m ()
setTitle = (title .=)

-- | Obtain the current timestamp. This is a function in case I want to change the
-- implementation in the future.
getGlobalTime ::
  MonadReader (World wm) m
  => m Timestamp
getGlobalTime = asks _globalTime

tickGlobalTime :: 
  MonadWorld wm m
  => Bool
  -> m ()
--I have no idea what my plans were for this flag.
tickGlobalTime False = dirtyTime .= True
tickGlobalTime True = do
  dirtyTime .= False
  _ <- globalTime <%= (+1)
  pass
  -- debug (bformat ("Dong. The time is now " %! int %! ".") r)


{-
addBaseActions
  :: HasLookingProperties s
  => World s
  -> World s
addBaseActions = foldr (.) id [
    addAction lookingActionImpl
  ]
-}