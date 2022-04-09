{-# LANGUAGE RoleAnnotations #-}

module Yaifl.World where
  import Solitude
  import Yaifl.Logger
  import Yaifl.Common
  import {-# SOURCE #-} Yaifl.Actions.Action
  import {-# SOURCE #-} Yaifl.Objects.Dynamic
  import Yaifl.Objects.ObjectData
  import Yaifl.Say
  import {-# SOURCE #-} Yaifl.Rulebooks.Rulebook
  import {-# SOURCE #-} Yaifl.Activities.Activity
  
  type role World nominal
  data World (wm :: WorldModel)
  -- data WorldModel (s :: Type) (d :: Type) (v :: Type) (o :: Type) 
  type MonadWorld wm m = (MonadReader (World wm) m, MonadState (World wm) m, Logger m)
  things :: Lens' (World wm) (Store (AbstractObject wm ThingData))
  rooms :: Lens' (World wm) (Store (AbstractObject wm RoomData))
  actions :: Lens' (World wm) (Map Text (Action wm))
  activities :: Lens' (World wm) (ActivityCollection wm)
  previousRoom :: Lens' (World wm) Entity
  firstRoom :: Lens' (World wm) (Maybe Entity)
  currentPlayer :: Lens' (World wm) Entity
  actionProcessing :: Lens' (World wm) (ActionProcessing wm)

  getGlobalTime :: MonadReader (World wm) m => m Timestamp
  tickGlobalTime :: MonadWorld wm m => Bool -> m ()
  
  whenPlayBegins :: Lens' (World wm) (Rulebook wm () () Bool)

  instance HasBuffer (World wm) 'SayBuffer 