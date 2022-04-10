

module Yaifl.WorldInfo 
  ( -- * Types
  World
  , MonadWorld
   -- * Modifying the world
  , getGlobalTime
  , tickGlobalTime
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
  --, addBaseActions
  , activities
  , currentPlayer
  ) where
 
import {-# SOURCE #-} Yaifl.World