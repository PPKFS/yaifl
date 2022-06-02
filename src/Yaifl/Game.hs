-- ~\~ language=Haskell filename=src/Yaifl/Game.hs
-- ~\~ begin <<lit/construction.md|src/Yaifl/Game.hs>>[0] project://lit/construction.md:7

{-# LANGUAGE ImplicitParams #-}

module Yaifl.Game
  ( runGame
  , EffStack

  ) where

import Katip
import Solitude hiding (runState)
import Yaifl.World
import Yaifl.Logger
import qualified Data.Text.Lazy.Builder as TLB
import Control.Exception (bracket)
import Cleff.State

type EffStack wm = '[Log, State (World wm), IOE]

runGame :: 
  Text 
  -> Eff (EffStack s) a
  -> World s
  -> IO a
runGame t f w = do
  (r, _) <- runIOE $ runState w $ runAndIgnoreLogging $ f
  return r

    --bracket makeLogEnv closeScribes $ \le -> do
    --  let initialContext = () -- this context will be attached to every log in your app and merged w/ subsequent contexts
    --  evalStateT (runKatipContextT le initialContext (Namespace [t]) (unGame f)) i
-- ~\~ end
