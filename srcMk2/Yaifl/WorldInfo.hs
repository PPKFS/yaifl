

module Yaifl.WorldInfo 
  ( -- * Types
  World
  , MonadWorld
   -- * Modifying the world
  , getGlobalTime
  , tickGlobalTime
  , whenConstructingM
    -- * Lenses
  --, title
  , darknessWitnessed
  , roomDescriptions
  , actions
  , actionProcessing
  , things
  , rooms
  , previousRoom
  , firstRoom
  , whenPlayBegins
  , currentStage
  , activities
  , currentPlayer
  ) where
 
import {-# SOURCE #-} Yaifl.World