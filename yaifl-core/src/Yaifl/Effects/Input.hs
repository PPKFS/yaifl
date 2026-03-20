{-|
Module      : Yaifl.Effects.Input
Copyright   : (c) Avery 2022-2026
License     : MIT
Maintainer  : ppkfs@outlook.com

Input effect for handling user input.
-}

module Yaifl.Effects.Input
  ( Input(..)
  , waitForInput
  ) where

import Yaifl.Prelude

import Effectful.TH

-- | Provides functionality for receiving user input.
data Input :: Effect where
  -- | Wait for user input and return it as optional text.
  -- This operation blocks until input is received.
  WaitForInput :: Input m (Maybe Text)

makeEffect ''Input