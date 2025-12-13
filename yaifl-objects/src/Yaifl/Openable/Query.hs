module Yaifl.Openable.Query
  ( getOpenabilityMaybe
  , setOpenability
  , modifyOpenability
  , isOpen
  , isClosed
  , isLocked
  , isUnlocked
  , openIt
  , closeIt
  ) where

import Yaifl.Prelude

import Yaifl.Core.Effects
import Yaifl.Core.Kinds.AnyObject
import Yaifl.Core.Kinds.Thing
import Yaifl.Core.Query.Property
import Yaifl.Core.TH
import Yaifl.Entity