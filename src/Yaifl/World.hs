-- ~\~ language=Haskell filename=src/Yaifl/World.hs
-- ~\~ begin <<lit/worldmodel/state.md|src/Yaifl/World.hs>>[0] project://lit/worldmodel/state.md:9
{-|
Module      : Yaifl.World
Description : The monolithic record state that runs everything.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE TemplateHaskell #-}
module Yaifl.World where
import Solitude
import Yaifl.Common

import Yaifl.Say
--import Yaifl.Rulebooks.Rulebook
--import Yaifl.Activities.Activity
--import Yaifl.Actions.Action
import Yaifl.Objects.Dynamic
--import Yaifl.Actions.Looking
--import Yaifl.Actions.Going

data World (wm :: WorldModel) = World
  { _worldMetadata :: Metadata wm
  , _worldStores :: WorldStores wm
  , _worldActions :: WorldActions wm
  , _messageBuffer :: MessageBuffer
  }

-- ~\~ begin <<lit/worldmodel/state.md|world-stores>>[0] project://lit/worldmodel/state.md:98
data WorldStores (wm :: WorldModel) = WorldStores
  { _entityCounter :: (Entity, Entity)
  , _things :: Store (AbstractThing wm)
  , _rooms :: Store (AbstractRoom wm)
  , _values :: Map Text (WMValues wm)
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  }
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/state.md|world-actions>>[0] project://lit/worldmodel/state.md:112

data WorldActions (wm :: WorldModel) = WorldActions
  { _actions :: () -- !(Map Text (Action wm))
  , _activities :: () -- !(ActivityCollection wm)
  , _whenPlayBegins :: () -- !(Rulebook wm () () Bool)
  , _actionProcessing :: ()-- ActionProcessing wm
  }

-- ~\~ end

makeLenses ''World
makeLenses ''WorldModel

-- ~\~ begin <<lit/worldmodel/state.md|world-other>>[0] project://lit/worldmodel/state.md:140

{-
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
-}
-- ~\~ end
-- ~\~ end
