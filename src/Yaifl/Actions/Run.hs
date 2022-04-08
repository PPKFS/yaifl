module Yaifl.Actions.Run where

addAction :: 
  Action wm 
  -> World wm 
  -> World wm 
addAction ac =
  actions % at (_actionName ac) ?~ ac

-- | Attempt to run an action from a text command (so will handle the parsing).
-- Note that this does require the arguments to be parsed out.
tryAction :: 
  MonadWorld wm m
  => Text -- ^ text of command
  -> (Timestamp -> UnverifiedArgs s) -- ^ Arguments without a timestamp
  -> m Bool
tryAction an f = do
  ta <- getGlobalTime
  debug (bformat ("Trying to do the action '" %! stext %! "'" ) an)
  let uva = f ta
  ac <- getAction an uva
  case ac of
    Nothing -> err (bformat ("Couldn't find a matching action for '" %! stext %! "'") an) >> return False
    Just a -> fromMaybe False <$> runAction uva a

-- | Run an action. This assumes that all parsing has been completed.
runAction :: 
  MonadWorld wm m
  => WithThings wm s
  => UnverifiedArgs s
  -> Action wm 
  -> m (Maybe Bool)
runAction args act = do
  w <- get
  let (ActionProcessing ap) = _actionProcessing w
  ap act args
