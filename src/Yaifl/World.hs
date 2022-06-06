-- ~\~ language=Haskell filename=src/Yaifl/World.hs
-- ~\~ begin <<lit/worldmodel/state.md|src/Yaifl/World.hs>>[0] project://lit/worldmodel/state.md:9

{-# LANGUAGE TemplateHaskell #-}
module Yaifl.World where
import Solitude
import Yaifl.Common

import Yaifl.Say
--import Yaifl.Rulebooks.Rulebook
--import Yaifl.Activities.Activity
--import Yaifl.Actions.Action
import Yaifl.Objects.Dynamic
import Yaifl.Objects.Query
import Cleff.State
import Yaifl.Objects.Object
import Yaifl.Objects.Create
import Display
--import Yaifl.Actions.Looking
--import Yaifl.Actions.Going

data World (wm :: WorldModel) = World
  { _worldMetadata :: Metadata wm
  , _worldStores :: WorldStores wm
  , _worldActions :: WorldActions wm
  , _messageBuffer :: MessageBuffer
  }

-- ~\~ begin <<lit/worldmodel/state.md|world-stores>>[0] project://lit/worldmodel/state.md:109
data WorldStores (wm :: WorldModel) = WorldStores
  { _entityCounter :: (Entity, Entity)
  , _things :: Store (AbstractThing wm)
  , _rooms :: Store (AbstractRoom wm)
  , _values :: Map Text (WMValues wm)
  , _concepts :: ()-- !(Store (AbstractConcept t r c))
  }
-- ~\~ end
-- ~\~ begin <<lit/worldmodel/state.md|world-actions>>[0] project://lit/worldmodel/state.md:123

data WorldActions (wm :: WorldModel) = WorldActions
  { _actions :: () -- !(Map Text (Action wm))
  , _activities :: () -- !(ActivityCollection wm)
  , _whenPlayBegins :: () -- !(Rulebook wm () () Bool)
  , _actionProcessing :: ()-- ActionProcessing wm
  }

-- ~\~ end

makeLenses ''World
makeLenses ''WorldModel
makeLenses ''WorldStores

-- ~\~ begin <<lit/worldmodel/state.md|world-other>>[0] project://lit/worldmodel/state.md:151
-- | Turn an `AbstractObject` into a regular `Object` and update the cache if needed.
reifyObject ::
  State (Metadata wm) :> es
  => (AbstractObject wm d -> Eff es ())
  -> AbstractObject wm d
  -> Eff es (Object wm d)
reifyObject _ (StaticObject v) = return v
reifyObject setFunc (DynamicObject ts) = do
  let co = _tsCachedObject ts
  now <- getGlobalTime
  if
    _tsCacheStamp ts == now
  then
    return co
  else
    do
      -- update the object
      updatedObj <- runObjectUpdate (_tsUpdateFunc ts) co
      t <- getGlobalTime
      setFunc (DynamicObject $ TimestampedObject updatedObj t (_tsUpdateFunc ts))
      return updatedObj

reifyRoom :: 
  State (Metadata wm) :> es
  => (ObjectCreation wm :> es)
  => AbstractRoom wm
  -> Eff es (Room wm)
reifyRoom = reifyObject addAbstractRoom

reifyThing :: 
  State (Metadata wm) :> es
  => (ObjectCreation wm :> es)
  => AbstractThing wm
  -> Eff es (Thing wm)
reifyThing = reifyObject addAbstractThing

runCreationAsLookup :: 
  State (World wm) :> es
  => Eff (ObjectCreation wm : es) 
  ~> Eff es
runCreationAsLookup = interpret \case
  GenerateEntity bThing -> if bThing then 
    ((worldStores % entityCounter % _1) <<%= (+1)) else ((worldStores % entityCounter % _2) <<%= (+1))
  AddAbstractRoom aRoom -> worldStores % rooms % at (getID aRoom) ?= aRoom
  AddAbstractThing aThing -> worldStores % things % at (getID aThing) ?= aThing

runQueryAsLookup :: 
  State (World wm) :> es
  => (ObjectCreation wm :> es)
  => (State (Metadata wm) :> es)
  => Eff (ObjectQuery wm : es) 
  ~> Eff es
runQueryAsLookup = interpret \case
  LookupThing e -> do
    mbObj <- use $ worldStores % things % at (getID e)
    case mbObj of
      Nothing -> return 
        if isThing e 
          then 
            Left $ "Tried to lookup a room as a thing " <> displayText (getID e) 
          else 
            Left $ "Could not find" <> displayText (getID e)
      Just ao -> withoutMissingObjects (Right <$> reifyThing ao) (\mo -> return $ Left $ "Failed to reify " <> displayText mo)
  LookupRoom e -> error ""
  SetRoom r -> error ""
  SetThing t -> error ""


{-


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
