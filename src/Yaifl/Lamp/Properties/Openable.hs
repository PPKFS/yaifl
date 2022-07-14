-- ~\~ language=Haskell filename=src/Yaifl/Core/Properties/Openable.hs
-- ~\~ begin <<lit/properties/std/openable.md|src/Yaifl/Core/Properties/Openable.hs>>[0] project://lit/properties/std/openable.md:4
{-# LANGUAGE TemplateHaskell #-}

module Yaifl.Lamp.Properties.Openable 
  ( -- * Types
    Openable(..)
  , getOpenable
  ) where

import Yaifl.Core.Properties.TH
import Yaifl.Core.Objects.Query
import Yaifl.Core.Properties.Property
import Yaifl.Core.Properties.Query
-- | Whether the thing is open or not.
data Openable = Open | Closed 
  deriving stock (Eq, Show, Read, Ord, Generic)

makeSpecificsWithout [] ''Openable
-- ~\~ end
