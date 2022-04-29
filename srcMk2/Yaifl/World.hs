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
    World(..)
  , MonadWorld
   -- * Modifying the world
  , newEntityID
  , setTitle
  , getGlobalTime
  , tickGlobalTime
  , whenConstructingM
    -- * Lenses
  , title
  , darknessWitnessed
  , roomDescriptions
  , actionProcessing
  , actions
  , things
  , rooms
  , previousRoom
  , firstRoom
  , whenPlayBegins
  , addBaseActions
  , activities
  , currentPlayer
  , currentStage
  ) where

import Solitude
import Yaifl.Common
import Yaifl.Say
import Yaifl.Rulebooks.Rulebook
import Yaifl.Logger
import Yaifl.Activities.Activity
import Yaifl.Actions.Action
import Yaifl.Objects.Dynamic
import Yaifl.Actions.Looking
import Yaifl.Actions.Going

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
  , _values :: !(Map Text (WMValues wm))
  , _previousRoom :: !Entity
  , _currentPlayer :: !Entity
  , _firstRoom :: !(Maybe Entity)
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  , _actions :: !(Map Text (Action wm))
  , _activities :: !(ActivityCollection wm)
  , _whenPlayBegins :: !(Rulebook wm () () Bool)
  , _messageBuffers :: !(MessageBuffer, MessageBuffer)
  , _actionProcessing :: ActionProcessing wm
  , _currentStage :: WorldStage
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

instance HasBuffer (World wm) 'SayBuffer where
  bufferL _ = messageBuffers % _1

addBaseActions :: 
  HasLookingProperties wm
  => World wm
  -> World wm
addBaseActions = foldr (.) id [
    addAction lookingActionImpl
  , addAction goingActionImpl
  ]

whenConstructingM :: 
  MonadWorld wm m 
  => m Bool 
  -> m () 
  -> m ()
whenConstructingM cond = 
  whenM (andM [do
    cs <- use currentStage
    return $ cs == Construction, cond])