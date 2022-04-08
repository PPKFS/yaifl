{-# LANGUAGE RoleAnnotations #-}

module Yaifl.World where
  import Solitude
  import Yaifl.Logger
  import Yaifl.Common
  import {-# SOURCE #-} Yaifl.Actions.Action
  import {-# SOURCE #-} Yaifl.Objects.Dynamic
  import Yaifl.Objects.ObjectData
  
  type role World nominal
  data World (wm :: WorldModel)
  -- data WorldModel (s :: Type) (d :: Type) (v :: Type) (o :: Type) 
  type MonadWorld wm m = (MonadReader (World wm) m, MonadState (World wm) m, Logger m)
  things :: Lens' (World wm) (Store (AbstractObject wm ThingData))
  rooms :: Lens' (World wm) (Store (AbstractObject wm RoomData))
  actions :: Lens' (World wm) (Map Text (Action wm))
  getGlobalTime :: MonadReader (World wm) m => m Timestamp
