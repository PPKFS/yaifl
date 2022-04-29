{-|
Module      : Yaifl.Game
Description : A concrete monad stack to actually run the game.
Copyright   : (c) Avery, 2022
License     : MIT
Maintainer  : ppkfs@outlook.com
Stability   : No
-}

{-# LANGUAGE ImplicitParams #-}

module Yaifl.Game
  ( Game(..)
  , runGame

  ) where
  
import Katip
import Solitude
import Yaifl.World
import Yaifl.Logger
import qualified Data.Text.Lazy.Builder as TLB
import Control.Exception (bracket)

-- | The monad stack we use to run the game.
newtype Game wm a = Game
  { unGame :: KatipContextT (StateT (World wm) IO) a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, Katip, KatipContext, MonadState (World wm))

-- | All 'KatipLogger's fulfill the logging interface described by 'Logger'.
instance Logger (Game wm) where
  debug = logItemM (toLoc ?callStack) DebugS . LogStr
  info = logItemM (toLoc ?callStack) InfoS . LogStr
  warn = logItemM (toLoc ?callStack) WarningS . LogStr
  err = logItemM (toLoc ?callStack) ErrorS . LogStr
  withContext n = katipAddNamespace (Namespace [toStrict $ TLB.toLazyText n])

instance MonadReader (World wm) (Game wm) where
  ask = get
  local f g = do
    s <- get
    put (f s)
    r <- g
    put s
    return r

runGame :: 
  Text 
  -> Game s a 
  -> World s 
  -> IO a
runGame t f i = do
  withFile "log.json" AppendMode \fh -> do
    handleScribe <- mkHandleScribeWithFormatter jsonFormatYaifl ColorIfTerminal fh (permitItem DebugS) V2
    let makeLogEnv = registerScribe "file" handleScribe defaultScribeSettings =<< initLogEnv "" ""
    -- closeScribes will stop accepting new logs, flush existing ones and clean up resources
    bracket makeLogEnv closeScribes $ \le -> do
      let initialContext = () -- this context will be attached to every log in your app and merged w/ subsequent contexts
      evalStateT (runKatipContextT le initialContext (Namespace [t]) (unGame f)) i