# The World State

This is the monolithic core of the library. It can be chunked into a few main pieces, each of which is handled by a separate effect handler to keep our separation of concerns as modular as possible (for instance, an object should not be able to run an `Action` when printing out some text!).

We put the message buffer as part of the state even though we are in `IO` because it allows us to directly consume the output (e.g. for testing).


```haskell file=src/Yaifl/World.hs
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
    World(..), MonadWorld
   -- * Modifying the world
  , newEntityID, setTitle, getGlobalTime, tickGlobalTime, whenConstructingM
    -- * Lenses
  , title, darknessWitnessed, roomDescriptions, actionProcessing, actions, things
  , rooms, previousRoom, firstRoom, whenPlayBegins, addBaseActions, activities
  , currentPlayer, currentStage
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

data World (wm :: WorldModel) = World
  { _worldMetadata :: Metadata wm
  , _worldStaging :: WorldStaging wm
  , _worldStores :: WorldStores wm
  , _worldGameState :: WorldGameState wm
  , _worldActions :: WorldActions wm
  , _messageBuffers :: (MessageBuffer, MessageBuffer)
  }

<<world-metadata>>
<<world-staging>>
<<world-stores>>
<<world-actions>>
<<world-game-state>>

makeLenses ''World
makeLenses ''WorldModel

<<world-other>>
```
---
## Metadata

Things that exist outside of any semantic game world we can think of - user settings, the title, and 'special' actions which exist outside of the world (what Inform calls "out-of-game actions", I think). These are read-only for in-game actions, and read-write for special actions.

```haskell id=world-metadata

data Metadata wm = Metadata
  { _title :: Text
  , _roomDescriptions :: RoomDescriptions
  -- more to come I guess
  }
```

### Room Descriptions

Lifted directly from Inform; this sets whether to always print room descriptions (No..) even if the room is visited, to only print them on the first entry (Sometimes..), or never.

```haskell id=room-descriptions
data RoomDescriptions = SometimesAbbreviatedRoomDescriptions
  | AbbreviatedRoomDescriptions
  | NoAbbreviatedRoomDescriptions 
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)
```

## World Staging

Fields that make it easier to build a world; for instance, by saving the last modified room (to implicitly place items). 

```haskell id=world-staging
data CurrentStage = Construction | Verification | Runtime
  deriving stock (Eq, Show, Read, Ord, Enum, Generic)

data WorldStaging (wm :: WorldModel) = WorldStaging
  { _currentStage :: CurrentStage
  , _previousRoom :: Entity
  , _firstRoom :: Entity
  }
```

## World Stores

The lookup tables for various objects, values, etc in the game. This is probably the most important part of the `World` state.

```haskell id=world-stores

data WorldStores (wm :: WorldModel) = WorldStores
  { _entityCounter :: (Entity, Entity)
  , _things :: Store (AbstractThing wm)
  , _rooms :: Store (AbstractRoom wm)
  , _values :: Map Text (WMValues wm)
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  }
```

## World Actions

These are the dynamic parts that run things. This is the in-world actions, the standard activities (because user-defined activities can be done separately and don't need a lookup), and the two standalone rulebooks. Again, user-defined rulebooks act the same as activities and don't need to be stored around in the state.

```haskell id=world-actions

data WorldActions (wm :: WorldModel) = WorldActions
  { _actions :: !(Map Text (Action wm))
  , _activities :: !(ActivityCollection wm)
  , _whenPlayBegins :: !(Rulebook wm () () Bool)
  , _actionProcessing :: ActionProcessing wm
  }
```

## Game State

Finally, we have the pieces of the game that are sort of metadata but are likely to change around at runtime; for example, who the current player is, or whether the dark room description text has been displayed. The `globalTime` is *separate* from the current turn counter. The turn counter is based on how many actions the player has taken. The `Timestamp` is a way to allow us to cache objects when nothing has changed since their last caching. 

```haskell id=world-game-state

data WorldGameState (wm :: WorldModel) = WorldGameState
  { _dirtyTime :: Bool
  , _globalTime :: Timestamp
  , _darknessWitnessed :: Bool
  , _currentPlayer :: Entity
  }
```

### Timestamp Caching

It is up to functions which might do some more complex processing (e.g. `move`) to update the time.

```haskell id=timestamp

newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock   (Show, Read, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)

```

---

# Other

```haskell id=world-other


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
```
