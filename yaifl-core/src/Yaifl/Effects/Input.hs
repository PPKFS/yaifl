-- | Input effect for handling user input in interactive applications.
--
-- This module provides the 'Input' effect which allows waiting for and receiving
-- user input. It's designed to be used with the effectful ecosystem for managing
-- side effects in a type-safe manner.
--
-- The effect supports:
-- * Waiting for user input
-- * Returning input as optional text (Nothing for EOF/empty input)
--
-- Example usage:
--
-- @
-- -- In an effectful computation
-- input <- waitForInput
-- case input of
--   Nothing -> say "No input received"
--   Just text -> say $ "You entered: " <> text
-- @

module Yaifl.Effects.Input
  ( Input(..)
  , waitForInput

  ) where

import Yaifl.Prelude

import Effectful.TH

-- | The 'Input' effect provides functionality for receiving user input.
--
-- This effect has a single operation 'WaitForInput' that suspends execution
-- until input is available, then returns it as optional text.
--
-- Type parameter 'm' represents the monad in which this effect is interpreted.

data Input :: Effect where
  -- | Wait for user input and return it as optional text.
  --
  -- Returns 'Nothing' if no input is available (EOF) or 'Just' the input text.
  -- This operation blocks until input is received.
  WaitForInput :: Input m (Maybe Text)

-- | Generate effectful boilerplate for the 'Input' effect.
-- This creates the necessary typeclass instances and functions
-- for using the effect in the effectful ecosystem.
makeEffect ''Input