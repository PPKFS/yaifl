module Yaifl.Effects.Input
  ( Input(..)
  , waitForInput

  ) where

import Yaifl.Prelude

import Effectful.TH

data Input :: Effect where
  WaitForInput :: Input m (Maybe Text)

makeEffect ''Input