{-# LANGUAGE RoleAnnotations #-}

module Yaifl.World where
  import Solitude
  import Yaifl.Logger
  type role World nominal
  data World (s :: Type)
  type MonadWorld s m = (MonadReader (World s) m, MonadState (World s) m, Logger m)