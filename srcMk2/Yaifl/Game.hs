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

