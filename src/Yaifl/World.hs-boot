module Yaifl.World where
  import Solitude
  import Yaifl.Logger
  data World s
  type MonadWorld s m = (MonadReader (World s) m, MonadState (World s) m, Logger m)